{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.List
import qualified Data.Map
import qualified DataTypes
import qualified Diagrams.Backend.SVG
import qualified LayoutEngine
import qualified Options.Applicative
import qualified Records
import qualified Renderer
import qualified System.Directory
import qualified System.IO

maxInputFileSizeInBytes :: Integer
maxInputFileSizeInBytes = 102400

handleReadError :: Control.Exception.IOException -> IO Data.ByteString.Lazy.ByteString
handleReadError e = return . Data.ByteString.Lazy.Char8.pack $ "Error reading file: " ++ show e

main :: IO ()
main = process =<< Options.Applicative.execParser options
  where
    options =
      Options.Applicative.info
        (Records.drakonRendererArguments Options.Applicative.<**> Options.Applicative.helper)
        (Options.Applicative.fullDesc
           <> Options.Applicative.progDesc "drakon renderer"
           <> Options.Applicative.header "drakon renderer")

correctNumberOfQuestionDependencies :: Int
correctNumberOfQuestionDependencies = 2

oneTitleIconPresent :: [Records.Icon] -> Maybe (String, String)
oneTitleIconPresent icons =
  if (1 :: Int)
       == foldl
            (\acc x ->
               case Records.getIconKind x of
                 DataTypes.Title -> acc + 1
                 _ -> acc)
            0
            icons
    then Nothing
    else Just
           ( "Diagram is required to have exactly one icon of kind \""
               ++ show DataTypes.Title
               ++ "\"."
           , "Make sure your input diagram contains an icon of kind \""
               ++ show DataTypes.Title
               ++ "\" and that it is the only icon of that kind.")

oneEndIconPresent :: [Records.Icon] -> Maybe (String, String)
oneEndIconPresent icons =
  if (1 :: Int)
       == foldl
            (\acc x ->
               case Records.getIconKind x of
                 DataTypes.End -> acc + 1
                 _ -> acc)
            0
            icons
    then Nothing
    else Just
           ( "Diagram is required to have exactly one icon of kind \""
               ++ show DataTypes.End
               ++ "\"."
           , "Make sure your input diagram contains an icon of kind \""
               ++ show DataTypes.End
               ++ "\" and that it is the only icon of that kind.")

-- 2024-09-06 PJ:
-----------------
-- TODO: adjust for the new kinds of icons (headline + address).
correctNumberOfDependencies :: [Records.Icon] -> Maybe (String, String)
correctNumberOfDependencies icons =
  case iconsWithIncorrectDependencies of
    [] -> Nothing
    _ ->
      Just
        ( take (length errorText - 2) errorText ++ "."
        , "Make sure your icons have the expected number of dependencies. For reference: \""
            ++ show DataTypes.Title
            ++ "\" and \""
            ++ show DataTypes.Action
            ++ "\" icons should have 1 depdenency, \""
            ++ show DataTypes.Question
            ++ "\" icon should have 2 dependencies and \""
            ++ show DataTypes.End
            ++ "\" should have no dependencies.")
  where
    iconsWithIncorrectDependencies =
      foldl
        (\acc x ->
           case Records.getIconKind x of
             DataTypes.Question ->
               if correctNumberOfQuestionDependencies /= Records.getNumberOfDependentIcons x
                 then Records.getIconName x : acc
                 else acc
             DataTypes.End ->
               if 0 /= Records.getNumberOfDependentIcons x
                 then Records.getIconName x : acc
                 else acc
             _ ->
               if 1 /= Records.getNumberOfDependentIcons x
                 then Records.getIconName x : acc
                 else acc)
        []
        icons
    errorText =
      foldl
        (\acc x -> acc ++ "\"" ++ x ++ "\", ")
        "Icons identified with following names contain incorrect number of dependencies: "
        iconsWithIncorrectDependencies

validation :: [[Records.Icon] -> Maybe (String, String)] -> [Records.Icon] -> [(String, String)]
validation validationPredicates icons =
  foldl
    (\acc predicate ->
       case predicate icons of
         Nothing -> acc
         Just (validationError, hint) -> (validationError, hint) : acc)
    []
    validationPredicates

combine :: [Records.Icon] -> [Records.Icon] -> [[Records.Icon]]
combine parents = foldl (\acc dependent -> acc ++ [dependent : parents]) []

pathsEqual :: [[Records.Icon]] -> [[Records.Icon]] -> Bool
pathsEqual x1 x2 =
  length x1 == length x2
    && foldl
         (\acc (x1', x2') -> length x1' == length x2' && null x1' || head x1' == head x2' && acc)
         True
         (zip x1 x2)

-- "paths" are represented like this (simplified):
-- [
--     [Icon6, Icon3],
--     [Icon6, Icon4, Icon3]
-- ]
-- and they represent all paths starting at a given divergence icon
-- and ending at a given convergence icon.
-- d-c paths - my name for divergence icon -> convergence icon paths
-- all paths connecting
dcPaths :: [[Records.Icon]] -> [Records.Icon] -> [Records.Icon] -> [[Records.Icon]]
dcPaths paths allIcons convergenceIcons =
  let newPaths =
        foldl
          (\acc singleRow ->
             acc
               ++ case Records.getDependentIconsWithBlacklist
                         (head singleRow)
                         allIcons
                         convergenceIcons
                         singleRow of
                    [] -> [singleRow]
                    dependents -> combine singleRow dependents)
          []
          paths
   in if pathsEqual paths newPaths
        then map reverse paths
        else dcPaths newPaths allIcons convergenceIcons

showBalancedPathsHeader :: [[Records.Icon]] -> String
showBalancedPathsHeader inputPaths =
  let header =
        foldl
          (\acc i ->
             case i of
               0 -> "\n| path " ++ show (i + 1) ++ " |"
               _ -> acc ++ " path " ++ show (i + 1) ++ " |")
          ""
          [0 .. (length inputPaths - 1)]
      headerLineBreak =
        foldl
          (\acc i ->
             case i of
               0 -> "\n| --- |"
               _ -> acc ++ " --- |")
          ""
          [0 .. (length inputPaths - 1)]
   in header ++ headerLineBreak

iconPresentFurtherDownAnotherPath :: [[Records.Icon]] -> Records.Icon -> Int -> Bool
iconPresentFurtherDownAnotherPath inputPaths icon rowIndex =
  let targetName = Records.getIconName icon
      lowerRows = drop (rowIndex + 1) inputPaths
   in any (any (\x -> Records.getIconName x == targetName)) lowerRows

shiftMarker :: String
shiftMarker = " :arrow_down: "

getIconMarker :: [[Records.Icon]] -> Records.Icon -> Int -> String
getIconMarker inputPaths icon rowIndex =
  if iconPresentFurtherDownAnotherPath inputPaths icon rowIndex
    then shiftMarker
    else " "

showBalancedPaths :: [[Records.Icon]] -> String
showBalancedPaths inputPaths =
  let maxColumnIndex = length (head inputPaths) - 1
      formatIcon row columnIndex =
        case drop columnIndex row of
          (icon:_) ->
            let name = Records.getIconName icon
                description = Records.getIconDescription icon
                dependents = show $ Records.getIconNamesOfDependentIcons icon
             in " **" ++ name ++ "** - " ++ description ++ " " ++ dependents ++ " |"
          [] -> " --- |"
   in concat
        [ "|" ++ concat [formatIcon row columnIndex | row <- inputPaths] ++ "\n"
        | columnIndex <- [0 .. maxColumnIndex]
        ]

skipFirst :: [[Records.Icon]] -> [[Records.Icon]]
skipFirst = foldl (\acc row -> acc ++ [drop 1 row]) []

takeFirst :: [[Records.Icon]] -> [[Records.Icon]]
takeFirst = foldl (\acc row -> acc ++ [take 1 row]) []

iconPresent :: Records.Icon -> [[Records.Icon]] -> Bool
iconPresent x = any (elem x)

sliceMap :: [[Records.Icon]] -> Data.Map.Map Records.Icon Records.Icon
sliceMap [] = Data.Map.empty
sliceMap input =
  foldl
    (\acc row ->
       case row of
         (icon:_) ->
           let currentIconPresent = iconPresent icon (skipFirst input)
               newIcon =
                 if currentIconPresent
                   then Records.valentPoint "0" ":new:"
                   else icon
            in Data.Map.insertWith const icon newIcon acc
         [] -> acc)
    Data.Map.empty
    input

balanceFirstSlice :: [[Records.Icon]] -> [[Records.Icon]]
balanceFirstSlice [] = []
balanceFirstSlice input =
  let rowMap = sliceMap input
   in (if Data.Map.null rowMap
         then input
         else foldl
                (\acc row ->
                   case row of
                     [] -> acc
                     (key:rest) ->
                       let value = rowMap Data.Map.! key
                        in if key == value
                             then acc ++ [key : rest]
                             else acc ++ [value : (key : rest)])
                []
                input)

balance :: [[Records.Icon]] -> [[Records.Icon]]
balance unbalanancedPaths =
  case balanceFirstSlice unbalanancedPaths of
    [] -> []
    result ->
      let firstBalancedSlice = takeFirst result
          remainingPaths = skipFirst result
       in if all null remainingPaths
            then firstBalancedSlice
            else zipWith (++) firstBalancedSlice (balance remainingPaths)

process :: Records.DrakonRendererArguments -> IO ()
process (Records.DrakonRendererArguments inputPath layoutOutputPath balancedPathsOutputPath svgOutputPath) = do
  fileSizeInBytes <- System.Directory.getFileSize inputPath
  if fileSizeInBytes > maxInputFileSizeInBytes
    then putStrLn
           $ "Problem with diagram file \""
               ++ inputPath
               ++ "\" ("
               ++ show fileSizeInBytes
               ++ " bytes). Max allowed input file size: "
               ++ show maxInputFileSizeInBytes
               ++ " bytes."
    else do
      content <- Control.Exception.catch (Data.ByteString.Lazy.readFile inputPath) handleReadError
      case Data.Aeson.decode content :: Maybe [Records.Icon] of
        Just icons -> do
          let validationErrors =
                validation
                  [oneTitleIconPresent, oneEndIconPresent, correctNumberOfDependencies]
                  icons
          case validationErrors of
            [] -> do
              titleIcon <-
                maybe
                  (fail
                     $ "No icons of type \"" ++ show DataTypes.Title ++ "\" detected in the input.")
                  return
                  (Records.titleIcon icons)
              endIcon <-
                maybe
                  (fail $ "No icons of type \"" ++ show DataTypes.End ++ "\" detected in the input.")
                  return
                  (Records.endIcon icons)
              let paths = dcPaths [[titleIcon]] icons [endIcon]
              putStrLn "dcPaths:"
              print paths
              let bPaths = balance paths
              let printableBPaths =
                    showBalancedPathsHeader bPaths ++ "\n" ++ showBalancedPaths bPaths
              let prettyMarkdown =
                    Data.ByteString.Lazy.Char8.pack $ "# balanced paths\n" ++ printableBPaths
              bPathsOutputHandle <- System.IO.openFile balancedPathsOutputPath System.IO.WriteMode
              Data.ByteString.Lazy.hPutStr bPathsOutputHandle prettyMarkdown
              System.IO.hClose bPathsOutputHandle
              let refreshedPositionedIcons = LayoutEngine.firstPaths titleIcon icons
              layoutOutputhandle <- System.IO.openFile layoutOutputPath System.IO.WriteMode
              Data.ByteString.Lazy.hPutStr
                layoutOutputhandle
                (Data.Aeson.Encode.Pretty.encodePretty refreshedPositionedIcons)
              System.IO.hClose layoutOutputhandle
              let thisIsJustTemporary = Renderer.renderAllConnections refreshedPositionedIcons
              Diagrams.Backend.SVG.renderSVG' svgOutputPath Renderer.svgOptions
                $ Renderer.renderAllIcons refreshedPositionedIcons <> snd thisIsJustTemporary
            _ -> do
              let failureReasons =
                    foldl
                      (\acc (validationError, hint) ->
                         acc ++ "* Error: " ++ validationError ++ " Hint: " ++ hint ++ "\n")
                      ""
                      validationErrors
              putStrLn
                $ "Input validation did not succeed for following reasons:\n" ++ failureReasons
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn
            $ "Problem interpreting diagram file \""
                ++ inputPath
                ++ "\". Details: "
                ++ unpackedContent
