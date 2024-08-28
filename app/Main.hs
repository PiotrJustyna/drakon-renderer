{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
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
          let graph = Records.directedGraph icons

          GHC.Utils.Outputable.printSDocLn
            GHC.Utils.Outputable.defaultSDocContext
            GHC.Utils.Ppr.LeftMode
            System.IO.stdout . GHC.Utils.Outputable.ppr $ graph

          handle <- System.IO.openFile textOutputPath System.IO.WriteMode

          let positionedIcons = LayoutEngine.cartesianPositioning graph

          Data.ByteString.Lazy.hPutStr handle (Data.Aeson.Encode.Pretty.encodePretty positionedIcons)

          System.IO.hClose handle

          Diagrams.Backend.SVG.renderSVG' svgOutputPath Renderer.svgOptions $ Renderer.renderAll positionedIcons
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem interpreting diagram file \"" ++ textInputPath ++ "\". Details: " ++ unpackedContent
