module Drakon.ValentPoint where

import Diagrams.Prelude (Point(..), V2(..), position, p2)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, troubleshootingMode)
import Drakon.HelperDiagrams (boundingBox, renderedConnection)
import Drakon.TypeClasses (Renderer(heightInUnits, render, widthInUnits))

newtype ValentPoint =
  ValentPoint (Point V2 Double)

instance Renderer ValentPoint where
  render valentPoint@(ValentPoint origin@(P (V2 x y1))) _ =
    let connectionX = x + defaultBoundingBoxWidth * 0.5
        y2 = y1 - defaultBoundingBoxHeight
    in (renderedConnection [p2 (connectionX, y1), p2 (connectionX, y2)])
        <> position
              [ ( origin
                , if troubleshootingMode
                    then boundingBox
                           (widthInUnits valentPoint * defaultBoundingBoxWidth)
                           (heightInUnits valentPoint * defaultBoundingBoxHeight)
                    else mempty)
              ]
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0
