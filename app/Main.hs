{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Colour.SRGB
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

-- 2024-08-26 PJ:
-----------------
-- This should ideally be moved somewhere else. Renderer module?
svgOptions :: Num n => Diagrams.Prelude.Options Diagrams.Backend.SVG.SVG Diagrams.Prelude.V2 n
svgOptions = Diagrams.Backend.SVG.SVGOptions {
  Diagrams.Backend.SVG._size = Diagrams.Prelude.mkSizeSpec $ Diagrams.Prelude.V2 (Just 400) (Just 400),
  Diagrams.Backend.SVG._idPrefix = Data.Text.empty,
  Diagrams.Backend.SVG._svgDefinitions = Nothing,
  Diagrams.Backend.SVG._svgAttributes = [],
  Diagrams.Backend.SVG._generateDoctype = True
}

iconWidth :: Double
iconWidth = 1.0

iconHeight :: Double
iconHeight = 0.5

lineColour :: Diagrams.Prelude.Colour Double
lineColour = Data.Colour.SRGB.sRGB (34.0/255.0) (69.0/255.0) (57.0/255.0)

titleIconColour :: Diagrams.Prelude.Colour Double
titleIconColour = Data.Colour.SRGB.sRGB (69.0/255.0) (173.0/255.0) (127.0/255.0)

fontColour :: Diagrams.Prelude.Colour Double
fontColour = Data.Colour.SRGB.sRGB (34.0/255.0) (69.0/255.0) (57.0/255.0)

fontSize :: Double
fontSize = 0.075

text :: String -> Double -> Double -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
text content translateX translateY =
  Diagrams.Prelude.text content
  Diagrams.Prelude.#
  Diagrams.Prelude.fontSize (Diagrams.Prelude.local fontSize)
  Diagrams.Prelude.#
  Diagrams.Prelude.light
  Diagrams.Prelude.#
  Diagrams.Prelude.font "helvetica"
  Diagrams.Prelude.#
  Diagrams.Prelude.fc fontColour
  Diagrams.Prelude.#
  Diagrams.Prelude.translate (Diagrams.Prelude.r2 (translateX,  translateY))

diagram :: Records.PositionedIcon -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
diagram _ =
  text "helo, world!" 0.0 0.0
  <> shape
  where
    shape =
      Diagrams.Prelude.roundedRect iconWidth iconHeight 0.5
      Diagrams.Prelude.#
      Diagrams.Prelude.fc titleIconColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

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

          -- 2024-08-26 PJ:
          -----------------
          -- For now just taking the first positioned icon.
          Diagrams.Backend.SVG.renderSVG' svgOutputPath svgOptions (diagram (head positionedIcons))
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem interpreting diagram file \"" ++ textInputPath ++ "\". Details: " ++ unpackedContent
