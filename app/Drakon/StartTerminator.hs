module Drakon.StartTerminator where

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

data StartTerminator
  = Title String
  | CyclicStart String
  | TitleWithParameters String
  | CyclicStartWithParameters String

instance Renderer StartTerminator where
  render title@(Title content) origin =
    position
      [ ( origin
        , renderText
            content
            (0.0 + widthInUnits title * defaultBoundingBoxWidth * 0.5)
            (0.0 - heightInUnits title * defaultBoundingBoxHeight * 0.5)
            <> (drakonStyle
                  (roundedRect
                     (widthInUnits title * defaultBoundingBoxWidth * widthRatio)
                     (heightInUnits title * defaultBoundingBoxHeight * 0.5)
                     0.5)
                  # translate (r2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-0.5))))
            <> if troubleshootingMode
                 then boundingBox
                        (widthInUnits title * defaultBoundingBoxWidth)
                        (heightInUnits title * defaultBoundingBoxHeight)
                 else mempty)
      ]
  render _ _ = mempty
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0
