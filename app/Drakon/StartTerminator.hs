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
import Drakon.Content (Content)
import Drakon.HelperDiagrams
import Drakon.ID (ID)
import Drakon.TypeClasses

data StartTerminator
  = Title ID (Point V2 Double) Content
  | CyclicStart ID (Point V2 Double) Content
  | TitleWithParameters ID (Point V2 Double) Content
  | CyclicStartWithParameters ID (Point V2 Double) Content

changeOrigin :: StartTerminator -> Point V2 Double -> StartTerminator
changeOrigin (Title id _ content) newOrigin = Title id newOrigin content
changeOrigin (CyclicStart id _ content) newOrigin = CyclicStart id newOrigin content
changeOrigin (TitleWithParameters id _ content) newOrigin = TitleWithParameters id newOrigin content
changeOrigin (CyclicStartWithParameters id _ content) newOrigin = CyclicStartWithParameters id newOrigin content

instance Renderer StartTerminator where
  render title@(Title titleId origin content) =
    position
      [ ( origin
        , renderText
            ((if troubleshootingMode
                then "[" <> show titleId <> " | " <> show origin <> "] "
                else "")
               <> show content)
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
  render _ = mempty
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0
