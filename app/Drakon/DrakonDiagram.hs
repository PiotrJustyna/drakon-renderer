module Drakon.DrakonDiagram where

import Data.Map (Map, empty, lookup)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..), p2)
import Drakon.Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Drakon.EndTerminator (EndTerminator, changeOrigin)
import Drakon.HelperDiagrams (renderedConnection)
import Drakon.ID (ID)
import Drakon.SkewerBlock (SkewerBlock, heightInUnits', position', renderIcons, toMap, widthInUnits')
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

renderSingleSkewer :: [SkewerBlock] -> Point V2 Double -> (Diagram B, Double)
renderSingleSkewer skewerBlocks origin@(P (V2 x y)) =
  let connectionX = x + defaultBoundingBoxWidth * 0.5
      skewerY = defaultBoundingBoxHeight
      startY1 = y - skewerY * 0.75
      startY2 = y - defaultBoundingBoxHeight
      positionedSkewerBlocks = position' skewerBlocks (p2 (x, y - skewerY))
      mapOfOrigins = toMap positionedSkewerBlocks
      renderedSkewerBlocks = renderIcons positionedSkewerBlocks mapOfOrigins
      finishY1 = y - skewerY - heightInUnits' positionedSkewerBlocks
      finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
   in ( renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)]
          <> renderedSkewerBlocks
          <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)]
      , finishY1)

instance Renderer DrakonDiagram where
  render diagram@(DrakonDiagram startTerminator allSkewers endTerminator) _ =
    let (result, finishY1, finishX) =
          if length allSkewers > 1
            then foldl
                   (\(accuResult, _, newSkewerOriginX) singleSkewer ->
                      let (newResult, finishY) = renderSingleSkewer singleSkewer (p2 (newSkewerOriginX, 0.0))
                       in ( accuResult <> newResult
                          , finishY
                          , newSkewerOriginX + defaultBoundingBoxWidth * widthInUnits' singleSkewer))
                   (mempty, 0.0, 0.0)
                   allSkewers
            else let (newResult, finishY) = renderSingleSkewer (head allSkewers) (p2 (0.0, 0.0))
                  in (newResult, finishY, 0.0)
     in render (Drakon.StartTerminator.changeOrigin startTerminator (P (V2 0.0 0.0))) empty
          <> result
          <> render
               (Drakon.EndTerminator.changeOrigin
                  endTerminator
                  (P (V2
                        (finishX
                           - defaultBoundingBoxWidth
                               * (if length allSkewers > 1
                                    then widthInUnits' (last allSkewers)
                                    else 0.0))
                        finishY1)))
               empty
  widthInUnits (DrakonDiagram _ allSkewers _) = maximum $ map widthInUnits' allSkewers
  heightInUnits (DrakonDiagram startTerminator allSkewers endTerminator) =
    sum $ heightInUnits startTerminator : heightInUnits endTerminator : map heightInUnits' allSkewers
