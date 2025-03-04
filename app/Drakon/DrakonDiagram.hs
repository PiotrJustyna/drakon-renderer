module Drakon.DrakonDiagram where

import Diagrams.Prelude (Point(..), V2(..), p2)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Drakon.EndTerminator (EndTerminator, changeOrigin)
import Drakon.HelperDiagrams (renderedConnection)
import Drakon.ID (ID)
import Drakon.SkewerBlock (SkewerBlock, render')
import Drakon.StartTerminator (StartTerminator, changeOrigin)
import Drakon.TypeClasses (Renderer(heightInUnits, render, widthInUnits))

data DrakonDiagram =
  DrakonDiagram StartTerminator [SkewerBlock] EndTerminator [((Double, Double), (Double, Double))]
  -- DrakonDiagram StartTerminator [SkewerBlock] EndTerminator [(ID, ID)]

instance Show DrakonDiagram where
  show diagram =
    "diagram total width in units: "
      <> show (widthInUnits diagram)
      <> "\n"
      <> "diagram total height in units: "
      <> show (heightInUnits diagram)

instance Renderer DrakonDiagram where
  render (DrakonDiagram startTerminator skewerBlocks endTerminator additionalConnections) =
    let origin@(P (V2 x y)) = p2 (0.0, 0.0)
        connectionX = x + widthInUnits startTerminator * defaultBoundingBoxWidth * 0.5
        skewerY = heightInUnits startTerminator * defaultBoundingBoxHeight
        startY1 = y - skewerY * 0.75
        startY2 = y - defaultBoundingBoxHeight
        renderedSkewerBlocks = render' skewerBlocks (p2 (x, y - skewerY))
        finishY1 = snd renderedSkewerBlocks
        finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
        renderedAdditionalConnections =
          foldl (\accu (start, finish) -> accu <> renderedConnection [p2 start, p2 finish]) mempty additionalConnections
     in render (Drakon.StartTerminator.changeOrigin startTerminator origin)
          <> renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)]
          <> fst renderedSkewerBlocks
          <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)]
          <> render (Drakon.EndTerminator.changeOrigin endTerminator (P (V2 x (snd renderedSkewerBlocks))))
          <> renderedAdditionalConnections
  widthInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator _additionalConnections) =
    maximum $ widthInUnits startTerminator : map widthInUnits skewerBlocks ++ [widthInUnits endTerminator]
  heightInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator _additionalConnections) =
    sum $ heightInUnits startTerminator : map heightInUnits skewerBlocks ++ [heightInUnits endTerminator]
