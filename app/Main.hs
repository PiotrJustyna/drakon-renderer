{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
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
              let paths = LayoutEngine.dcPaths [[titleIcon]] icons [endIcon]
              let bPaths = LayoutEngine.balance paths 1
              let printableBPaths =
                    LayoutEngine.showBalancedPathsHeader bPaths
                      ++ "\n"
                      ++ LayoutEngine.showBalancedPaths bPaths
              let prettyMarkdown =
                    Data.ByteString.Lazy.Char8.pack $ "# balanced paths\n" ++ printableBPaths
              bPathsOutputHandle <- System.IO.openFile balancedPathsOutputPath System.IO.WriteMode
              Data.ByteString.Lazy.hPutStr bPathsOutputHandle prettyMarkdown
              System.IO.hClose bPathsOutputHandle
              let refreshedPositionedIcons = LayoutEngine.positionIcons bPaths
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
