module Drakon.ValentPoint where

import Diagrams.Prelude (Point(..), V2(..), position)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, troubleshootingMode)
import Drakon.HelperDiagrams (boundingBox)
import Drakon.TypeClasses (Renderer(heightInUnits, render, widthInUnits))

newtype ValentPoint =
  ValentPoint (Point V2 Double)

instance Renderer ValentPoint where
  render valentPoint@(ValentPoint origin) _ =
    position
      [ ( origin
        , if troubleshootingMode
            then boundingBox
                   (widthInUnits valentPoint * defaultBoundingBoxWidth)
                   (heightInUnits valentPoint * defaultBoundingBoxHeight)
            else mempty)
      ]
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0
