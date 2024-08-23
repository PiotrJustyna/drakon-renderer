{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text
import qualified Diagrams.Backend.SVG
import qualified Diagrams.Prelude
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
import qualified LayoutEngine
import qualified Options.Applicative
import qualified Records
import qualified System.Directory
import qualified System.IO

svgOpt :: Num n => Diagrams.Prelude.Options Diagrams.Backend.SVG.SVG Diagrams.Prelude.V2 n
svgOpt = Diagrams.Backend.SVG.SVGOptions {
  Diagrams.Backend.SVG._size = Diagrams.Prelude.mkSizeSpec $ Diagrams.Prelude.V2 (Just 400) (Just 400),
  Diagrams.Backend.SVG._idPrefix = Data.Text.empty,
  Diagrams.Backend.SVG._svgDefinitions = Nothing,
  Diagrams.Backend.SVG._svgAttributes = [],
  Diagrams.Backend.SVG._generateDoctype = True
}

diagram :: Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
diagram =
  Diagrams.Prelude.circle 1
  Diagrams.Prelude.# Diagrams.Prelude.fc Diagrams.Prelude.orange
  Diagrams.Prelude.# Diagrams.Prelude.lw Diagrams.Prelude.ultraThick
  Diagrams.Prelude.# Diagrams.Prelude.lc Diagrams.Prelude.blue
  Diagrams.Prelude.# Diagrams.Prelude.frame 0.2

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

          Data.ByteString.Lazy.hPutStr handle (Data.Aeson.Encode.Pretty.encodePretty $ LayoutEngine.cartesianPositioning graph)

          System.IO.hClose handle
          
          -- 2024-08-23 PJ:
          -----------------
          -- For now we generate just a sample image.
          -- To be replaced but an actual rendering later.
          Diagrams.Backend.SVG.renderSVG' svgOutputPath svgOpt diagram
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem interpreting diagram file \"" ++ textInputPath ++ "\". Details: " ++ unpackedContent
