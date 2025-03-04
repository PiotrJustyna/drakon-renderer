module Drakon.ValentPoint where

import Diagrams.Prelude (Point(..), V2(..), position)
import Drakon.Constants
import Drakon.HelperDiagrams
import Drakon.TypeClasses

data ValentPoint = ValentPoint (Point V2 Double)

instance Renderer ValentPoint where
  render valentPoint@(ValentPoint origin) =
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
