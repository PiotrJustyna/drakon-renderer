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

renderSingleSkewer :: [SkewerBlock] -> Point V2 Double -> Double -> (Diagram B, Double)
renderSingleSkewer skewerBlocks origin@(P (V2 x y)) addressDepth =
  let connectionX = x + defaultBoundingBoxWidth * 0.5
      skewerY = defaultBoundingBoxHeight
      startY1 = y - skewerY
      startY2 = y - defaultBoundingBoxHeight
      positionedSkewerBlocks = position' skewerBlocks (p2 (x, y - skewerY)) addressDepth
      mapOfOrigins = toMap positionedSkewerBlocks
      renderedSkewerBlocks = renderIcons positionedSkewerBlocks mapOfOrigins
      finishY1 = y - skewerY - heightInUnits' positionedSkewerBlocks
      finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
   in ( renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)] <> renderedSkewerBlocks
          -- <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)] -- TODO: I don't think it belongs here but anxious to remove it yet.
      , finishY1)

instance Renderer DrakonDiagram where
  render diagram@(DrakonDiagram startTerminator allSkewers endTerminator) _ =
    let (result, _, finishY1, finishX) =
          if length allSkewers > 1
            then foldl
                   (\(accuResult, connectionToPreviousSkewer, _, skewerOriginX) singleSkewer ->
                      let (newResult, finishY) = renderSingleSkewer singleSkewer (p2 (skewerOriginX, 0.0)) (-19.0)
                          nextSkewerOriginX = skewerOriginX + defaultBoundingBoxWidth * widthInUnits' singleSkewer
                       in ( accuResult <> newResult <> connectionToPreviousSkewer
                          , renderedConnection
                              [ p2 (skewerOriginX + defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-1.0))
                              , p2
                                  (nextSkewerOriginX + defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-1.0))
                              ]
                            <> renderedConnection
                                [ p2 (skewerOriginX + defaultBoundingBoxWidth * 0.5, -19.0 - 1.0)
                                , p2 (nextSkewerOriginX + defaultBoundingBoxWidth * 0.5, -19.0 - 1.0)]
                          , finishY
                          , nextSkewerOriginX))
                   (mempty, mempty, 0.0, 0.0)
                   allSkewers
            else let (newResult, finishY) = renderSingleSkewer (head allSkewers) (p2 (0.0, 0.0)) (-19.0) -- TODO: this probably should be made optional or we need another function for silhouette diagrams
                  in (newResult, mempty, finishY, 0.0)
        endTerminatorXCoordinate = (finishX
           - defaultBoundingBoxWidth
               * (if length allSkewers > 1
                    then widthInUnits' (last allSkewers)
                    else 0.0))
     in render (Drakon.StartTerminator.changeOrigin startTerminator (P (V2 0.0 0.0))) empty
          <> (renderedConnection
                [ p2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-0.75))
                , p2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-1.0))
                ])
          <> result
          <> (renderedConnection
                [ p2 (endTerminatorXCoordinate + defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-19.0 - 1.0))
                , p2 (endTerminatorXCoordinate + defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-19.0 - 1.25))
                ])
          <> render
               (Drakon.EndTerminator.changeOrigin
                  endTerminator
                  (P (V2
                        endTerminatorXCoordinate
                        (-19.0 - 1.0))))
               empty
  widthInUnits (DrakonDiagram _ allSkewers _) = sum $ map widthInUnits' allSkewers
  heightInUnits (DrakonDiagram startTerminator allSkewers endTerminator) =
    (heightInUnits startTerminator) + (heightInUnits endTerminator) + (maximum $ map heightInUnits' allSkewers)
