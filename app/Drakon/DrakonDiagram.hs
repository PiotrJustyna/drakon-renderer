module Drakon.DrakonDiagram where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..), p2)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Drakon.EndTerminator (EndTerminator)
import Drakon.HelperDiagrams (renderedConnection)
import Drakon.SkewerBlock (SkewerBlock, heightInUnits', render', widthInUnits')
import Drakon.StartTerminator (StartTerminator)
import Drakon.TypeClasses (Renderer(..))

data DrakonDiagram =
  DrakonDiagram StartTerminator [SkewerBlock] EndTerminator

instance Show DrakonDiagram where
  show diagram =
    "diagram total width in units: "
      <> show (widthInUnits diagram)
      <> "\n"
      <> "diagram total height in units: "
      <> show (heightInUnits diagram)

instance Renderer DrakonDiagram where
  render (DrakonDiagram startTerminator skewerBlocks endTerminator) origin@(P (V2 x y)) =
    let connectionX = x + widthInUnits startTerminator * defaultBoundingBoxWidth * 0.5
        skewerY = heightInUnits startTerminator * defaultBoundingBoxHeight
        startY1 = y - skewerY * 0.75
        startY2 = y - defaultBoundingBoxHeight
        renderedSkewerBlocks = render' skewerBlocks (p2 (x, y - skewerY))
        finishY1 = snd renderedSkewerBlocks
        finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
     in render startTerminator origin
          <> renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)]
          <> fst renderedSkewerBlocks
          <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)]
          <> render endTerminator (P (V2 x (snd renderedSkewerBlocks)))
  widthInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator) =
    maximum $ widthInUnits startTerminator : map widthInUnits skewerBlocks ++ [widthInUnits endTerminator]
  heightInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator) =
    sum $ heightInUnits startTerminator : map heightInUnits skewerBlocks ++ [heightInUnits endTerminator]
