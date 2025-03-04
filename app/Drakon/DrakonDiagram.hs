module Drakon.DrakonDiagram where

import Diagrams.Prelude (Point(..), V2(..), p2)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Drakon.EndTerminator (EndTerminator)
import Drakon.HelperDiagrams (renderedConnection)
import Drakon.SkewerBlock (SkewerBlock, render')
import Drakon.StartTerminator (StartTerminator)
import Drakon.TypeClasses (Renderer(render, widthInUnits, heightInUnits))
import Drakon.ID (ID)

data DrakonDiagram =
    DrakonDiagram StartTerminator [SkewerBlock] EndTerminator [((Double, Double), (Double, Double))]

instance Show DrakonDiagram where
  show diagram =
    "diagram total width in units: "
      <> show (widthInUnits diagram)
      <> "\n"
      <> "diagram total height in units: "
      <> show (heightInUnits diagram)

instance Renderer DrakonDiagram where
  render (DrakonDiagram startTerminator skewerBlocks endTerminator additionalConnections) origin@(P (V2 x y)) =
    let connectionX = x + widthInUnits startTerminator * defaultBoundingBoxWidth * 0.5
        skewerY = heightInUnits startTerminator * defaultBoundingBoxHeight
        startY1 = y - skewerY * 0.75
        startY2 = y - defaultBoundingBoxHeight
        renderedSkewerBlocks = render' skewerBlocks (p2 (x, y - skewerY))
        finishY1 = snd renderedSkewerBlocks
        finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
        renderedAdditionalConnections = foldl
          (\accu (start, finish) -> accu <> renderedConnection [p2 start, p2 finish])
          mempty
          additionalConnections
     in render startTerminator origin
          <> renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)]
          <> fst renderedSkewerBlocks
          <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)]
          <> render endTerminator (P (V2 x (snd renderedSkewerBlocks)))
          <> renderedAdditionalConnections
  widthInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator _additionalConnections) =
    maximum $ widthInUnits startTerminator : map widthInUnits skewerBlocks ++ [widthInUnits endTerminator]
  heightInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator _additionalConnections) =
    sum $ heightInUnits startTerminator : map heightInUnits skewerBlocks ++ [heightInUnits endTerminator]
