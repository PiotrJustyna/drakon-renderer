module Renderer where

import Data.Bifunctor (second)
import Data.Colour.SRGB (sRGB)
import Data.Text (empty)
import DataTypes (IconKind(..))
import Diagrams.Backend.SVG
  ( B
  , Options(SVGOptions)
  , SVG
  , _generateDoctype
  , _idPrefix
  , _size
  , _svgAttributes
  , _svgDefinitions
  )
import Diagrams.Prelude
  ( Colour
  , Diagram
  , Point
  , V2(..)
  , (#)
  , circle
  , closeLine
  , fc
  , font
  , fontSize
  , fromOffsets
  , fromVertices
  , lc
  , light
  , local
  , lw
  , mkSizeSpec
  , p2
  , position
  , r2
  , rect
  , rotateBy
  , roundedRect
  , strokeLoop
  , text
  , translate
  , triangle
  , veryThin
  )
import Records
  ( PositionedIcon(PositionedIcon)
  , getDependentPositionedIcons
  , getIconDescription
  , getIconKind
  , getPositionedIconPositionX
  , getPositionedIconPositionY
  , icon
  , iconPositionX
  , iconPositionY
  )

svgOptions :: Num n => Options SVG V2 n
svgOptions =
  SVGOptions
    { _size = mkSizeSpec $ V2 (Just 1000) (Just 1000)
    , _idPrefix = empty
    , _svgDefinitions = Nothing
    , _svgAttributes = []
    , _generateDoctype = True
    }

iconWidth :: Double
iconWidth = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

iconHeight :: Double
iconHeight = 0.5

lineColour :: Colour Double
lineColour = sRGB (34.0 / 255.0) (69.0 / 255.0) (57.0 / 255.0)

fillColour :: Colour Double
fillColour = lineColour

titleIconColour :: Colour Double
titleIconColour = sRGB (69.0 / 255.0) (173.0 / 255.0) (127.0 / 255.0)

endIconColour :: Colour Double
endIconColour = titleIconColour

actionIconColour :: Colour Double
actionIconColour = titleIconColour

questionIconColour :: Colour Double
questionIconColour = titleIconColour

fontColour :: Colour Double
fontColour = sRGB (34.0 / 255.0) (69.0 / 255.0) (57.0 / 255.0)

defaultFontSize :: Double
defaultFontSize = 0.075

renderText :: String -> Double -> Double -> Diagram B
renderText content translateX translateY =
  text content
    # fontSize (local defaultFontSize)
    # light
    # font "helvetica"
    # fc fontColour
    # translate (r2 (translateX, translateY))

addIfNotContains :: (Double, Double) -> [(Double, Double)] -> [(Double, Double)]
addIfNotContains (x1, y1) z =
  if any (\(x2, y2) -> x1 == x2 && y1 == y2) z
    then z
    else (x1, y1) : z

connection ::
     (Double, Double) -> (Double, Double) -> [PositionedIcon] -> ([(Double, Double)], Diagram B)
connection (x1, y1) (x2, y2) positionedIcons
  | x1 == x2 =
    let waypoints =
          (x1, y1)
            : (if iconClash
                 then [ (x1 + iconWidth + spaceBetweenIconsX, y1)
                      , (x1 + iconWidth + spaceBetweenIconsX, y2 + iconHeight)
                      , (x1, y2 + iconHeight)
                      ]
                 else [])
                <> [(x2, y2)]
     in (waypoints, renderedConnection waypoints)
  | x1 < x2 =
    let waypoints = [(x1, y1), (x2, y1), (x2, y2)]
     in (waypoints, renderedConnection waypoints)
  | otherwise =
    if y1 < y2
      then let waypoints =
                 [ (x1, y1)
                 , (x1, y1 - iconHeight)
                 , (x1 + iconWidth + spaceBetweenIconsX, y1 - iconHeight)
                 , (x1 + iconWidth + spaceBetweenIconsX, y2 + iconHeight)
                 , (x2, y2 + iconHeight)
                 ]
            in ( waypoints
               , renderedConnection waypoints
                   <> rotateBy (1 / 4) (triangle 0.1)
                        #
          -- 0.087:   from Pythegorean theorem
          -- 0.0165:  from line width?
                         translate (r2 (x2 + 0.087 / 2.0 + 0.0165, y2 + iconHeight))
                        # lc lineColour
                        # lw veryThin
                        # fc fillColour)
      else if y1 == y2
             then let waypoints =
                        [ (x1, y1)
                        , (x1, y1 - iconHeight)
                        , (x1 - iconWidth - spaceBetweenIconsX, y1 - iconHeight)
                        , (x1 - iconWidth - spaceBetweenIconsX, y2 + iconHeight)
                        , (x2, y2 + iconHeight)
                        ]
                   in (waypoints, renderedConnection waypoints)
             else let waypoints = [(x1, y1), (x1, y2 + iconHeight), (x2, y2 + iconHeight), (x2, y2)]
                   in (waypoints, renderedConnection waypoints)
  where
    iconClash =
      any
        (\positionedIcon ->
           x1 == getPositionedIconPositionX positionedIcon
             && y1 > getPositionedIconPositionY positionedIcon
             && y2 < getPositionedIconPositionY positionedIcon)
        positionedIcons

renderedConnection :: [(Double, Double)] -> Diagram B
renderedConnection coordinatesOfNewLine =
  fromVertices (map p2 coordinatesOfNewLine) # lc lineColour # lw veryThin

renderSingleConnection ::
     PositionedIcon
  -> PositionedIcon
  -> [PositionedIcon]
  -> [[(Double, Double)]]
  -> ([[(Double, Double)]], Diagram B)
renderSingleConnection PositionedIcon {icon = _, iconPositionX = x1, iconPositionY = y1} PositionedIcon { icon = _
                                                                                                        , iconPositionX = x2
                                                                                                        , iconPositionY = y2
                                                                                                        } positionedIcons coordinatesOfTakenLines =
  (coordinatesOfNewLine : coordinatesOfTakenLines, renderedNewLine)
  where
    coordinatesOfNewLine = fst connectionInfo
    renderedNewLine = snd connectionInfo
    connectionInfo = connection (x1, y1) (x2, y2) positionedIcons

renderConnections ::
     PositionedIcon
  -> [PositionedIcon]
  -> [PositionedIcon]
  -> [[(Double, Double)]]
  -> ([[(Double, Double)]], Diagram B)
renderConnections _ [] _ coordinatesOfTakenLines = (coordinatesOfTakenLines, mempty)
renderConnections parent (d:ds) allPositionedIcons coordinatesOfTakenLines =
  second (snd dResult <>) dsResult
  where
    dResult = renderSingleConnection parent d allPositionedIcons coordinatesOfTakenLines
    dsResult = renderConnections parent ds allPositionedIcons (fst dResult)

renderAllConnections' ::
     [PositionedIcon]
  -> [PositionedIcon]
  -> [[(Double, Double)]]
  -> ([[(Double, Double)]], Diagram B)
renderAllConnections' [] _ coordinatesOfTakenLines = (coordinatesOfTakenLines, mempty)
renderAllConnections' (p:ps) allParents coordinatesOfTakenLines = second (snd pResult <>) psResult
  where
    pResult =
      renderConnections
        p
        (getDependentPositionedIcons p allParents)
        allParents
        coordinatesOfTakenLines
    psResult = renderAllConnections' ps allParents (fst pResult)

renderAllConnections :: [PositionedIcon] -> ([[(Double, Double)]], Diagram B)
renderAllConnections parents = renderAllConnections' parents parents []

renderAllIcons :: [PositionedIcon] -> Diagram B
renderAllIcons positionedIcons = position $ map renderSingleIcon positionedIcons

renderSingleIcon :: PositionedIcon -> (Point V2 Double, Diagram B)
renderSingleIcon PositionedIcon {icon = positionedIcon, iconPositionX = x, iconPositionY = y} =
  case kind of
    Title -> (coordinates, renderText description 0.0 0.0 <> titleShape)
    End -> (coordinates, renderText description 0.0 0.0 <> endShape)
    Action -> (coordinates, renderText description 0.0 0.0 <> actionShape)
    Question ->
      ( coordinates
      , renderText description 0.0 0.0
          <> renderText "yes" (iconWidth * (-0.1)) (iconHeight * (-0.7))
          <> renderText "no" (iconWidth * 0.55) (iconHeight * 0.15)
          <> questionShape)
    Headline -> (coordinates, renderText description 0.0 0.0 <> headlineShape)
    Address -> (coordinates, renderText description 0.0 0.0 <> addressShape)
    ForStart -> (coordinates, renderText description 0.0 0.0 <> forStartShape)
    ForEnd -> (coordinates, renderText description 0.0 0.0 <> forEndShape)
    ValentPoint -> (coordinates, renderText description 0.0 0.0 <> valentPointShape)
    Choice -> (coordinates, renderText description 0.0 0.0 <> choiceShape)
    Case -> (coordinates, renderText description 0.0 0.0 <> caseShape)
  where
    coordinates = p2 (x, y)
    kind = getIconKind positionedIcon
    description = getIconDescription positionedIcon
    titleShape =
      roundedRect iconWidth iconHeight 0.5 # fc titleIconColour # lc lineColour # lw veryThin
    valentPointShape = circle 0.1 # fc titleIconColour # lc lineColour # lw veryThin
    endShape = roundedRect iconWidth iconHeight 0.5 # fc endIconColour # lc lineColour # lw veryThin
    actionShape = rect iconWidth iconHeight # fc actionIconColour # lc lineColour # lw veryThin
    forStartShape =
      fromOffsets
        [ V2 0.0 (iconHeight * 0.5)
        , V2 (iconHeight * 0.5) (iconHeight * 0.5)
        , V2 (iconWidth - (iconHeight * 0.5) - (iconHeight * 0.5)) 0.0
        , V2 (iconHeight * 0.5) (iconHeight * (-0.5))
        , V2 0.0 (iconHeight * (-0.5))
        , V2 (iconWidth * (-1.0)) 0.0
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 (iconWidth * (-0.5), iconHeight * (-0.5)))
    forEndShape =
      fromOffsets
        [ V2 (iconHeight * (-0.5)) (iconHeight * 0.5)
        , V2 0.0 (iconHeight * 0.5)
        , V2 iconWidth 0.0
        , V2 0.0 (iconHeight * (-0.5))
        , V2 (iconHeight * (-0.5)) (iconHeight * (-0.5))
        , V2 (iconWidth * (-0.5)) 0.0
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 (iconWidth * (-0.5) + (iconHeight * 0.5), iconHeight * (-0.5)))
    choiceShape =
      fromOffsets
        [ V2 0.1 iconHeight
        , V2 iconWidth 0.0
        , V2 (-0.1) (iconHeight * (-1.0))
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 (iconWidth * (-0.5), iconHeight * (-0.5)))
    caseShape =
      (fromOffsets
        [ V2 iconWidth 0.0
        , V2 0.0 (iconHeight * (-1.0))
        , V2 (iconWidth * (-1.0)) 0.0
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 (iconWidth * (-0.5), iconHeight * (0.5))))
      <>
      (fromOffsets
        [ V2 iconWidth 0.0
        , V2 (iconWidth * (-0.5)) (-0.1)
        , V2 (iconWidth * (-0.5)) 0.1
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 (iconWidth * (-0.5), iconHeight * (-0.5))))
    questionShape =
      fromOffsets
        [ V2 (-0.1) (iconHeight * 0.5)
        , V2 0.1 (iconHeight * 0.5)
        , V2 (iconWidth - 0.1 - 0.1) 0.0
        , V2 0.1 (iconHeight * (-0.5))
        , V2 (-0.1) (iconHeight * (-0.5))
        , V2 ((iconWidth - 0.1 - 0.1) * (-1.0)) 0.0
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 ((iconWidth - 0.1 - 0.1) * (-0.5), iconHeight * (-0.5)))
    headlineShape =
      fromOffsets
        [ V2 0.0 iconHeight
        , V2 iconWidth 0.0
        , V2 0.0 (iconHeight * (-1.0))
        , V2 (iconWidth * (-0.5)) (-0.1)
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 (iconWidth * (-0.5), iconHeight * (-0.5)))
    addressShape =
      fromOffsets
        [ V2 0.0 iconHeight
        , V2 (iconWidth * 0.5) 0.1
        , V2 (iconWidth * 0.5) (-0.1)
        , V2 0.0 (iconHeight * (-1.0))
        , V2 (iconWidth * (-1.0)) 0
        ]
        # closeLine
        # strokeLoop
        # fc questionIconColour
        # lc lineColour
        # lw veryThin
        # translate (r2 (iconWidth * (-0.5), iconHeight * (-0.5)))
