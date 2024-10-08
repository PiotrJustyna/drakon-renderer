module Renderer where

import qualified Data.Bifunctor
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

fillColour :: Diagrams.Prelude.Colour Double
fillColour = lineColour

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
  Diagrams.Prelude.translate (Diagrams.Prelude.r2 (translateX, translateY))

addIfNotContains :: (Double, Double) -> [(Double, Double)] -> [(Double, Double)]
addIfNotContains (x1, y1) z = if any (\(x2, y2) -> x1 == x2 && y1 == y2) z then z else (x1, y1):z

startToFinishWaypoints :: (Double, Double) -> (Double, Double) -> [Records.PositionedIcon] -> [(Double, Double)]
startToFinishWaypoints (x1, y1) (x2, y2) positionedIcons
  | x1 == x2  = (x1, y1) : (if iconClash then [(x1 + iconWidth, y1), (x1 + iconWidth, y2 + iconHeight), (x1, y2 + iconHeight)] else []) ++ [(x2, y2)]
  | x1 < x2   = [(x1, y1), (x2, y1), (x2, y2)]
  | otherwise =
    if y1 < y2
      then [
        (x1, y1),
        (x1, y1 - iconHeight),
        (x1 + iconWidth, y1 - iconHeight),
        (x1 + iconWidth, y2 + iconHeight),
        (x2, y2 + iconHeight)]
      else [(x1, y1), (x1, y2 + iconHeight), (x2, y2 + iconHeight), (x2, y2)]
  where
    iconClash = any (\positionedIcon ->
      x1 == Records.getPositionedIconPositionX positionedIcon &&
      y1 > Records.getPositionedIconPositionY positionedIcon &&
      y2 < Records.getPositionedIconPositionY positionedIcon) positionedIcons

renderSingleConnection ::
  Records.PositionedIcon ->
  Records.PositionedIcon ->
  [Records.PositionedIcon] ->
  [[(Double, Double)]] ->
  ([[(Double, Double)]], Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B)
renderSingleConnection
  Records.PositionedIcon {
    Records.icon = _,
    Records.iconPositionX = x1,
    Records.iconPositionY = y1 }
  Records.PositionedIcon {
    Records.icon = _,
    Records.iconPositionX = x2,
    Records.iconPositionY = y2 }
  positionedIcons
  coordinatesOfTakenLines = (coordinatesOfNewLine:coordinatesOfTakenLines, renderedNewLine)
  where
    coordinatesOfNewLine = startToFinishWaypoints (x1, y1) (x2, y2) positionedIcons
    renderedNewLine = (Diagrams.Prelude.fromVertices (map Diagrams.Prelude.p2 coordinatesOfNewLine)
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.veryThin)
      -- <>
      -- (Diagrams.Prelude.rotateBy (1/4) $ Diagrams.Prelude.triangle 1)
      -- Diagrams.Prelude.#
      -- Diagrams.Prelude.translate (Diagrams.Prelude.r2 (x2, y2))
      -- Diagrams.Prelude.#
      -- Diagrams.Prelude.fc fillColour

renderConnections :: Records.PositionedIcon -> [Records.PositionedIcon] -> [Records.PositionedIcon] -> [[(Double, Double)]] -> ([[(Double, Double)]], Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B)
renderConnections _ [] _ coordinatesOfTakenLines = (coordinatesOfTakenLines, mempty)
renderConnections parent (d:ds) allPositionedIcons coordinatesOfTakenLines =
  Data.Bifunctor.second (snd dResult <>) dsResult
  where
    dResult   = renderSingleConnection parent d allPositionedIcons coordinatesOfTakenLines
    dsResult  = renderConnections parent ds allPositionedIcons (fst dResult)

renderAllConnections' :: [Records.PositionedIcon] -> [Records.PositionedIcon] -> [[(Double, Double)]] -> ([[(Double, Double)]], Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B)
renderAllConnections' [] _ coordinatesOfTakenLines = (coordinatesOfTakenLines, mempty)
renderAllConnections' (p:ps) allParents coordinatesOfTakenLines =
  Data.Bifunctor.second (snd pResult <>) psResult
  where
    pResult   = renderConnections p (Records.getDependentPositionedIcons p allParents) allParents coordinatesOfTakenLines
    psResult  = renderAllConnections' ps allParents (fst pResult)

renderAllConnections :: [Records.PositionedIcon] -> ([[(Double, Double)]], Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B)
renderAllConnections parents = renderAllConnections' parents parents []

renderAllIcons :: [Records.PositionedIcon] -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B
renderAllIcons positionedIcons = Diagrams.Prelude.position $ map renderSingleIcon positionedIcons

renderSingleIcon :: Records.PositionedIcon -> (Diagrams.Prelude.Point Diagrams.Prelude.V2 Double, Diagrams.Prelude.Diagram Diagrams.Backend.SVG.B)
renderSingleIcon Records.PositionedIcon {
  Records.icon = positionedIcon,
  Records.iconPositionX = x,
  Records.iconPositionY = y } =
  case kind of
    DataTypes.Title -> (coordinates, text description 0.0 0.0 <> titleShape)
    DataTypes.End -> (coordinates, text description 0.0 0.0 <> endShape)
    DataTypes.Action -> (coordinates, text description 0.0 0.0 <> actionShape)
    DataTypes.Question -> (coordinates, text description 0.0 0.0 <> text "yes" (iconWidth * (-0.1)) (iconHeight * (-0.7)) <> text "no" (iconWidth * 0.55) (iconHeight * 0.15) <> questionShape)
    DataTypes.Headline -> (coordinates, text description 0.0 0.0 <> headlineShape)
    DataTypes.Address -> (coordinates, text description 0.0 0.0 <> addressShape)
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
    headlineShape =
      Diagrams.Prelude.fromOffsets
      [Diagrams.Prelude.V2 0.0 iconHeight,
      Diagrams.Prelude.V2 iconWidth 0.0,
      Diagrams.Prelude.V2 0.0 (iconHeight * (-1.0)),
      Diagrams.Prelude.V2 (iconWidth * (-0.5)) (-0.1)]
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
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (iconWidth * (-0.5), iconHeight * (-0.5)))
    addressShape =
      Diagrams.Prelude.fromOffsets
      [Diagrams.Prelude.V2 0.0 iconHeight,
      Diagrams.Prelude.V2 (iconWidth * 0.5) 0.1,
      Diagrams.Prelude.V2 (iconWidth * 0.5) (-0.1),
      Diagrams.Prelude.V2 0.0 (iconHeight * (-1.0)),
      Diagrams.Prelude.V2 (iconWidth * (-1.0)) 0]
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
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (iconWidth * (-0.5), iconHeight * (-0.5)))
