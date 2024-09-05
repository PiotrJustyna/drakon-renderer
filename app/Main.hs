{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified DataTypes
import qualified Diagrams.Backend.SVG
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
import qualified LayoutEngine
import qualified Options.Applicative
import qualified Records
import qualified Renderer
import qualified System.Directory
import qualified System.IO

maxInputFileSizeInBytes :: Integer
maxInputFileSizeInBytes = 102400

handleReadError ::
  Control.Exception.IOException ->
  IO Data.ByteString.Lazy.ByteString
handleReadError e = return . Data.ByteString.Lazy.Char8.pack $ "Error reading file: " ++ show e

main :: IO ()
main = process =<< Options.Applicative.execParser options
  where
    options = Options.Applicative.info (Records.drakonRendererArguments Options.Applicative.<**> Options.Applicative.helper)
      (Options.Applicative.fullDesc
      <> Options.Applicative.progDesc "drakon renderer"
      <> Options.Applicative.header "drakon renderer")

oneTitleIconPresent :: [Records.Icon] -> Maybe (String, String)
oneTitleIconPresent icons =
  if  (1 :: Int) == foldl (\acc x ->
    case Records.getIconKind x of
      DataTypes.Title -> acc + 1
      _ -> acc) 0 icons
    then
      Nothing
    else Just (
      "The diagram is required to have exactly one icon of kind \"" ++ show DataTypes.Title ++ "\".",
      "Make sure your input diagram contains an icon of kind \"" ++ show DataTypes.Title ++ "\" and that it is the only icon of that kind.")

correctNumberOfDependencies :: Int
correctNumberOfDependencies = 2

correctNumberOfDependenciesInQuestionIcons :: [Records.Icon] -> Maybe (String, String)
correctNumberOfDependenciesInQuestionIcons icons =
  if any (\x ->
      case Records.getIconKind x of
        DataTypes.Question -> correctNumberOfDependencies == Records.getNumberOfDependentIcons x
        _ -> False) icons
    then
      Nothing
    else Just (
      "The diagram contains one or more icon of kind \"" ++ show DataTypes.Question ++ "\" containing an incorrect number of dependencies.",
      "Check that all icons of kind \"" ++ show DataTypes.Question ++ "\" have exactly " ++ show correctNumberOfDependencies ++ " dependencies.")

validation :: [[Records.Icon] -> Maybe (String, String)] -> [Records.Icon] -> [(String, String)]
validation validationPredicates icons =
  foldl (\acc predicate ->
    case predicate icons of
      Nothing -> acc
      Just (validationError, hint) -> (validationError, hint):acc) [] validationPredicates

process :: Records.DrakonRendererArguments -> IO ()
process (Records.DrakonRendererArguments textInputPath textOutputPath svgOutputPath) = do
  fileSizeInBytes <- System.Directory.getFileSize textInputPath

  if fileSizeInBytes > maxInputFileSizeInBytes
    then
      putStrLn $ "Problem with diagram file \"" ++ textInputPath ++ "\" (" ++ show fileSizeInBytes ++ " bytes). Max allowed input file size: " ++ show maxInputFileSizeInBytes ++ " bytes."
    else do
      content <- Control.Exception.catch (Data.ByteString.Lazy.readFile textInputPath) handleReadError

      case Data.Aeson.decode content :: Maybe [Records.Icon] of
        Just icons -> do
          let validationErrors = validation
                [oneTitleIconPresent,
                correctNumberOfDependenciesInQuestionIcons]
                icons

          case validationErrors of
            [] -> do
              let graph = Records.directedGraph icons

              GHC.Utils.Outputable.printSDocLn
                GHC.Utils.Outputable.defaultSDocContext
                GHC.Utils.Ppr.LeftMode
                System.IO.stdout . GHC.Utils.Outputable.ppr $ graph

              handle <- System.IO.openFile textOutputPath System.IO.WriteMode

              let positionedIcons = LayoutEngine.cartesianPositioning graph

              Data.ByteString.Lazy.hPutStr handle (Data.Aeson.Encode.Pretty.encodePretty positionedIcons)

              System.IO.hClose handle

              Diagrams.Backend.SVG.renderSVG' svgOutputPath Renderer.svgOptions $
                Renderer.renderAllIcons positionedIcons
                <>
                Renderer.renderAllConnections positionedIcons
            _ -> do
              let failureReasons = foldl (\acc (validationError, hint) -> acc ++ "* Error: " ++ validationError ++ " Hint: " ++ hint ++ "\n") "" validationErrors
              putStrLn $ "Input validation did not succeed for the following reasons:\n" ++ failureReasons
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem interpreting diagram file \"" ++ textInputPath ++ "\". Details: " ++ unpackedContent
