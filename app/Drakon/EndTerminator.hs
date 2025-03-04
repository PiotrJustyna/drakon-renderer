module Drakon.EndTerminator where

import Diagrams.Prelude
  ((#)
  , position
  , r2
  , roundedRect
  , translate)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, troubleshootingMode, widthRatio, drakonStyle)
import Drakon.HelperDiagrams (boundingBox, renderText)
import Drakon.TypeClasses (Renderer(render, widthInUnits, heightInUnits))

newtype EndTerminator =
  End String

instance Renderer EndTerminator where
  render end@(End content) origin =
    position
      [ ( origin
        , renderText
            content
            (0.0 + widthInUnits end * defaultBoundingBoxWidth * 0.5)
            (0.0 - heightInUnits end * defaultBoundingBoxHeight * 0.5)
            <> (drakonStyle
                  (roundedRect
                     (widthInUnits end * defaultBoundingBoxWidth * widthRatio)
                     (heightInUnits end * defaultBoundingBoxHeight * 0.5)
                     0.5)
                  # translate (r2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-0.5))))
            <> if troubleshootingMode
                 then boundingBox
                        (widthInUnits end * defaultBoundingBoxWidth)
                        (heightInUnits end * defaultBoundingBoxHeight)
                 else mempty)
      ]
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0
