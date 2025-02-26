module Drakon.ValentPoint where

import Diagrams.Prelude (position)
import Drakon.Constants
import Drakon.HelperDiagrams
import Drakon.TypeClasses

data ValentPoint =
  ValentPoint

instance Renderer ValentPoint where
  render ValentPoint origin =
    position
      [ ( origin
        , if troubleshootingMode
            then boundingBox
                   (widthInUnits ValentPoint * defaultBoundingBoxWidth)
                   (heightInUnits ValentPoint * defaultBoundingBoxHeight)
            else mempty)
      ]
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0
