module Renderer where

import qualified Data.Colour.SRGB
import qualified Data.Text
import qualified DataTypes
import qualified Diagrams.Backend.SVG
import qualified Diagrams.Prelude
import qualified Records

svgOptions :: Num n => Diagrams.Prelude.Options Diagrams.Backend.SVG.SVG Diagrams.Prelude.V2 n
svgOptions = Diagrams.Backend.SVG.SVGOptions {
  Diagrams.Backend.SVG._size = Diagrams.Prelude.mkSizeSpec $ Diagrams.Prelude.V2 (Just 1000) (Just 1000),
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

endIconColour :: Diagrams.Prelude.Colour Double
endIconColour = titleIconColour

actionIconColour :: Diagrams.Prelude.Colour Double
actionIconColour = titleIconColour

questionIconColour :: Diagrams.Prelude.Colour Double
questionIconColour = titleIconColour

fontColour :: Diagrams.Prelude.Colour Double
fontColour = Data.Colour.SRGB.sRGB (34.0/255.0) (69.0/255.0) (57.0/255.0)

fontSize :: Double
fontSize = 0.075

text :: String -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
text content =
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
  Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0.0 :: Double,  0.0 :: Double))

renderSingleConnection :: Records.PositionedIcon -> Records.PositionedIcon -> Double -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
renderSingleConnection
  Records.PositionedIcon {
    Records.icon = _,
    Records.iconPositionX = x1,
    Records.iconPositionY = y1 }
  Records.PositionedIcon {
    Records.icon = _,
    Records.iconPositionX = x2,
    Records.iconPositionY = y2 }
  minY
  | x1 <= x2 =
    Diagrams.Prelude.fromVertices (map Diagrams.Prelude.p2 [(x1, y1), (x2, y1), (x2, y2)])
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.veryThin
  | x1 > x2 =
    Diagrams.Prelude.fromVertices (map Diagrams.Prelude.p2 [(x1, y1), (x1, minY - iconHeight), (x2 + iconWidth, minY - iconHeight), (x2 + iconWidth, y2 + iconHeight), (x2, y2 + iconHeight), (x2, y2)])
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.veryThin
  | otherwise =
    Diagrams.Prelude.fromVertices $ map Diagrams.Prelude.p2 [(x1, y1), (x2, y2)]

renderConnections :: Records.PositionedIcon -> [Records.PositionedIcon] -> Double -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
renderConnections _ [] _        = mempty
renderConnections x [y] minY    = renderSingleConnection x y minY
renderConnections x (y:ys) minY = renderSingleConnection x y minY <> renderConnections x ys minY

renderAllConnections :: [Records.PositionedIcon] -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
renderAllConnections allPositionedIcons =
  foldl
    (<>)
    mempty
    [renderConnections x (Records.getDependentPositionedIcons x allPositionedIcons) minY | x <- allPositionedIcons]
  where
    minY = foldl (\acc Records.PositionedIcon { Records.icon = _, Records.iconPositionX = _, Records.iconPositionY = y } -> min acc y) 0 allPositionedIcons

renderAllIcons :: [Records.PositionedIcon] -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
renderAllIcons positionedIcons = Diagrams.Prelude.position $ map renderSingleIcon positionedIcons

renderSingleIcon :: Records.PositionedIcon -> (Diagrams.Prelude.Point Diagrams.Prelude.V2 Double, Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B)
renderSingleIcon Records.PositionedIcon {
  Records.icon = positionedIcon,
  Records.iconPositionX = x,
  Records.iconPositionY = y } =
  case kind of
    DataTypes.Title -> (coordinates, text description <> titleShape)
    DataTypes.End -> (coordinates, text description <> endShape)
    DataTypes.Action -> (coordinates, text description <> actionShape)
    DataTypes.Question -> (coordinates, text description <> questionShape)
  where
    coordinates = Diagrams.Prelude.p2 (x, y)
    kind = Records.getIconKind positionedIcon
    description = Records.getIconDescription positionedIcon
    titleShape =
      Diagrams.Prelude.roundedRect iconWidth iconHeight 0.5
      Diagrams.Prelude.#
      Diagrams.Prelude.fc titleIconColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.veryThin
    endShape =
      Diagrams.Prelude.roundedRect iconWidth iconHeight 0.5
      Diagrams.Prelude.#
      Diagrams.Prelude.fc endIconColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.veryThin
    actionShape =
      Diagrams.Prelude.rect iconWidth iconHeight
      Diagrams.Prelude.#
      Diagrams.Prelude.fc actionIconColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.veryThin
    questionShape =
      Diagrams.Prelude.fromOffsets
      [Diagrams.Prelude.V2 (-0.1) (iconHeight * 0.5),
      Diagrams.Prelude.V2 0.1 (iconHeight * 0.5),
      Diagrams.Prelude.V2 (iconWidth - 0.1 - 0.1) 0.0,
      Diagrams.Prelude.V2 0.1 (iconHeight * (-0.5)),
      Diagrams.Prelude.V2 (-0.1) (iconHeight * (-0.5)),
      Diagrams.Prelude.V2 ((iconWidth - 0.1 - 0.1) * (-1.0)) 0.0]
      Diagrams.Prelude.#
      Diagrams.Prelude.closeLine
      Diagrams.Prelude.#
      Diagrams.Prelude.strokeLoop
      Diagrams.Prelude.#
      Diagrams.Prelude.fc questionIconColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.veryThin
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 ((iconWidth - 0.1 - 0.1) * (-0.5), iconHeight * (-0.5)))
