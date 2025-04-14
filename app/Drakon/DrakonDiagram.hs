module Drakon.DrakonDiagram where

import Data.Map (Map, lookup)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..), p2)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Drakon.EndTerminator (EndTerminator, changeOrigin)
import Drakon.HelperDiagrams (renderedConnection)
import Drakon.ID (ID)
import Drakon.SkewerBlock (SkewerBlock, heightInUnits', widthInUnits', position', renderIcons, toMap)
import Drakon.StartTerminator (StartTerminator, changeOrigin)
import Drakon.TypeClasses (Renderer(heightInUnits, render, widthInUnits))

data DrakonDiagram =
  DrakonDiagram StartTerminator [[SkewerBlock]] EndTerminator

instance Show DrakonDiagram where
  show diagram =
    "diagram total width in units: "
      <> show (widthInUnits diagram)
      <> "\n"
      <> "diagram total height in units: "
      <> show (heightInUnits diagram)

instance Renderer DrakonDiagram where
  render (DrakonDiagram startTerminator allSkewers endTerminator) _ =
    let skewerBlocks = head allSkewers
        origin@(P (V2 x y)) = p2 (0.0, 0.0)
        connectionX = x + widthInUnits startTerminator * defaultBoundingBoxWidth * 0.5
        skewerY = heightInUnits startTerminator * defaultBoundingBoxHeight
        startY1 = y - skewerY * 0.75
        startY2 = y - defaultBoundingBoxHeight
        positionedSkewerBlocks = position' skewerBlocks (p2 (x, y - skewerY))
        mapOfOrigins = toMap positionedSkewerBlocks
        renderedSkewerBlocks = renderIcons positionedSkewerBlocks mapOfOrigins
        finishY1 = y - skewerY - heightInUnits' positionedSkewerBlocks
        finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
     in render (Drakon.StartTerminator.changeOrigin startTerminator origin) mapOfOrigins
          <> renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)]
          <> renderedSkewerBlocks
          <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)]
          <> render (Drakon.EndTerminator.changeOrigin endTerminator (P (V2 x finishY1))) mapOfOrigins
  widthInUnits (DrakonDiagram startTerminator allSkewers endTerminator) =
    maximum $ widthInUnits startTerminator : map widthInUnits' allSkewers ++ [widthInUnits endTerminator]
  heightInUnits (DrakonDiagram startTerminator allSkewers endTerminator) =
    sum $ heightInUnits startTerminator : map heightInUnits' allSkewers ++ [heightInUnits endTerminator]
