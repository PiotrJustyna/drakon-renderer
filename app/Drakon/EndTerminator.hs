module Drakon.EndTerminator where

import Diagrams.Prelude (Point(..), V2(..), (#), position, r2, roundedRect, translate)
import Drakon.Constants
  ( defaultBoundingBoxHeight
  , defaultBoundingBoxWidth
  , drakonStyle
  , troubleshootingMode
  , widthRatio
  )
import Drakon.Content (Content)
import Drakon.HelperDiagrams (boundingBox, renderText)
import Drakon.ID (ID)
import Drakon.TypeClasses (Renderer(heightInUnits, render, widthInUnits))

data EndTerminator =
  End ID (Point V2 Double) Content
  deriving Show

changeOrigin :: EndTerminator -> Point V2 Double -> EndTerminator
changeOrigin (End endId _ content) newOrigin = End endId newOrigin content

instance Renderer EndTerminator where
  render end@(End endId origin content) _ =
    position
      [ ( origin
        , renderText
            ((if troubleshootingMode
                then "[" <> show endId <> " | " <> show origin <> "] "
                else "")
               <> show content)
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
