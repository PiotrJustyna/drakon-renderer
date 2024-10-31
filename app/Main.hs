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

mapOfDependents :: [Records.Icon] -> Data.Map.Map String [String]
mapOfDependents =
  foldl
    (\acc icon ->
       let parentName = Records.getIconName icon
           dependentNames = Records.getIconNamesOfDependentIcons icon
        in Data.Map.insertWith (++) parentName dependentNames acc)
    Data.Map.empty

mapOfParents :: [Records.Icon] -> Data.Map.Map String [String]
mapOfParents =
  foldl
    (\acc1 icon ->
       let parentName = Records.getIconName icon
           dependentNames = Records.getIconNamesOfDependentIcons icon
        in foldl
             (\acc2 dependentName -> Data.Map.insertWith (++) dependentName [parentName] acc2)
             acc1
             dependentNames)
    Data.Map.empty

multipleValues :: Data.Map.Map String [String] -> [Records.Icon] -> [Records.Icon]
multipleValues x allIcons =
  let multipleValuesPerKey =
        Data.Map.foldrWithKey
          (\k _ acc -> k : acc)
          []
          (Data.Map.filter (\values -> length values > 1) x)
   in filter (\x -> Records.getIconName x `elem` multipleValuesPerKey) allIcons

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
                         convergenceIcons of
                    [] -> [singleRow]
                    dependents -> combine singleRow dependents)
          []
          paths
   in if pathsEqual paths newPaths
        then paths
        else dcPaths newPaths allIcons convergenceIcons

-- 1. valent points should have unique names
--    need a path identifier
--    need to introduce illegal id symbols (#?)
delta :: [Records.Icon] -> [Records.Icon] -> [Records.Icon]
delta x1 x2 =
  if l1 < l2
    then ((head x1) : valentPoints) ++ newTail
    else x1
  where
    l1 = length x1
    l2 = length x2
    oldTail = (tail x1)
    newTail =
      (Records.updateDependent
         (head oldTail)
         (Records.getIconName (head x1))
         ((Records.getIconName (last x1)) ++ "#1"))
        : (tail oldTail)
    valentPoints =
      foldr
        (\deltaIndex acc ->
           acc
             ++ [ Records.valentPoint
                    ((Records.getIconName (last x1)) ++ "#" ++ (show deltaIndex))
                    (case acc of
                       [] -> Records.getIconName (head x1)
                       otherwise -> Records.getIconName (head acc))
                ])
        []
        [1 .. (l2 - l1)]

dcPathWithValentPoints :: [[Records.Icon]] -> [[Records.Icon]]
dcPathWithValentPoints inputPaths =
  foldl (\acc x -> (delta x (head acc)) : acc) [(head sortedPaths)] (tail sortedPaths)
  where
    sortedPaths =
      Data.List.sortBy
        (\singlePath1 singlePath2 -> flip compare (length singlePath1) (length singlePath2))
        inputPaths

onlyValentPoints :: [[Records.Icon]] -> [Records.Icon]
onlyValentPoints =
  foldl
    (\acc1 singlePath ->
       acc1
         ++ foldl
              (\acc2 singleIcon ->
                 case Records.getIconKind singleIcon of
                   DataTypes.ValentPoint -> singleIcon : acc2
                   _ -> acc2)
              []
              singlePath)
    []

process :: Records.DrakonRendererArguments -> IO ()
process (Records.DrakonRendererArguments textInputPath textOutputPath svgOutputPath) = do
  fileSizeInBytes <- System.Directory.getFileSize textInputPath
  if fileSizeInBytes > maxInputFileSizeInBytes
    then putStrLn
           $ "Problem with diagram file \""
               ++ textInputPath
               ++ "\" ("
               ++ show fileSizeInBytes
               ++ " bytes). Max allowed input file size: "
               ++ show maxInputFileSizeInBytes
               ++ " bytes."
    else do
      content <-
        Control.Exception.catch (Data.ByteString.Lazy.readFile textInputPath) handleReadError
      case Data.Aeson.decode content :: Maybe [Records.Icon] of
        Just icons -> do
          let parents = mapOfParents icons
          let dependents = mapOfDependents icons
          let divergenceIcons = multipleValues dependents icons
          let convergenceIcons = multipleValues parents icons
          let paths = dcPaths [[head divergenceIcons]] icons (convergenceIcons)
          let pathsWithValentPoints = dcPathWithValentPoints paths
          let valentPoints = onlyValentPoints pathsWithValentPoints
          -- putStrLn "divergence icons:"
          -- print divergenceIcons
          -- putStrLn "convergence icons:"
          -- print convergenceIcons
          -- putStrLn "paths:"
          -- print paths
          -- putStrLn "paths with valent points:"
          -- print pathsWithValentPoints
          putStrLn "only valent points:"
          print valentPoints
          let iconsWithValentPoints = icons ++ valentPoints
          let validationErrors =
                validation
                  [oneTitleIconPresent, oneEndIconPresent, correctNumberOfDependencies]
                  iconsWithValentPoints
          case validationErrors of
            [] -> do
              case Records.titleIcon iconsWithValentPoints of
                Just titleIcon -> do
                  let refreshedPositionedIcons = LayoutEngine.firstPaths titleIcon iconsWithValentPoints
                  handle <- System.IO.openFile textOutputPath System.IO.WriteMode
                  Data.ByteString.Lazy.hPutStr
                    handle
                    (Data.Aeson.Encode.Pretty.encodePretty refreshedPositionedIcons)
                  System.IO.hClose handle
                  let thisIsJustTemporary = Renderer.renderAllConnections refreshedPositionedIcons
                  Diagrams.Backend.SVG.renderSVG' svgOutputPath Renderer.svgOptions
                    $ Renderer.renderAllIcons refreshedPositionedIcons <> snd thisIsJustTemporary
                Nothing ->
                  putStrLn
                    $ "No icons of type \"" ++ show DataTypes.Title ++ "\" detected in the input."
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
                ++ textInputPath
                ++ "\". Details: "
                ++ unpackedContent
