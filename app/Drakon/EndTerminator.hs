module Drakon.EndTerminator where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude
  ( Diagram
  , Point
  , V2(..)
  , (#)
  , closeLine
  , fc
  , font
  , fontSize
  , fromOffsets
  , lc
  , light
  , local
  , lw
  , origin
  , position
  , r2
  , roundedRect
  , strokeLoop
  , text
  , translate
  , veryThin
  )
import Drakon.Constants
import Drakon.HelperDiagrams
import Drakon.TypeClasses

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
