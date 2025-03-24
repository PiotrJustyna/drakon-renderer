module Drakon.DrakonDiagram where

import Data.Map (Map, lookup)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..), p2)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Drakon.EndTerminator (EndTerminator, changeOrigin)
import Drakon.HelperDiagrams (renderedConnection)
import Drakon.ID (ID)
import Drakon.SkewerBlock (SkewerBlock, heightInUnits', position', renderIcons, toMap)
import Drakon.StartTerminator (StartTerminator, changeOrigin)
import Drakon.TypeClasses (Renderer(heightInUnits, render, widthInUnits))

data DrakonDiagram =
  DrakonDiagram StartTerminator [SkewerBlock] EndTerminator [(ID, ID)]

instance Show DrakonDiagram where
  show diagram =
    "diagram total width in units: "
      <> show (widthInUnits diagram)
      <> "\n"
      <> "diagram total height in units: "
      <> show (heightInUnits diagram)

renderAdditionalConnections :: Map ID (Point V2 Double) -> [(ID, ID)] -> Diagram B
renderAdditionalConnections mapOfOrigins =
  foldl
    (\accu (startID, finishID) ->
       let start = Data.Map.lookup startID mapOfOrigins
           finish = Data.Map.lookup finishID mapOfOrigins
        in case (start, finish) of
             (Just start', Just finish') -> accu <> renderedConnection [start', finish']
             _ -> accu)
    mempty

instance Renderer DrakonDiagram where
  render (DrakonDiagram startTerminator skewerBlocks endTerminator additionalConnections) =
    let origin@(P (V2 x y)) = p2 (0.0, 0.0)
        connectionX = x + widthInUnits startTerminator * defaultBoundingBoxWidth * 0.5
        skewerY = heightInUnits startTerminator * defaultBoundingBoxHeight
        startY1 = y - skewerY * 0.75
        startY2 = y - defaultBoundingBoxHeight
        positionedSkewerBlocks = position' skewerBlocks (p2 (x, y - skewerY))
        mapOfOrigins = toMap positionedSkewerBlocks
        renderedSkewerBlocks = renderIcons positionedSkewerBlocks
        finishY1 = y - skewerY - heightInUnits' positionedSkewerBlocks
        finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
     in render (Drakon.StartTerminator.changeOrigin startTerminator origin)
          <> renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)]
          <> renderedSkewerBlocks
          <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)]
          <> render (Drakon.EndTerminator.changeOrigin endTerminator (P (V2 x finishY1)))
          <> renderAdditionalConnections mapOfOrigins additionalConnections
  widthInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator _additionalConnections) =
    maximum $ widthInUnits startTerminator : map widthInUnits skewerBlocks ++ [widthInUnits endTerminator]
  heightInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator _additionalConnections) =
    sum $ heightInUnits startTerminator : map heightInUnits skewerBlocks ++ [heightInUnits endTerminator]
