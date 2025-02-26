module Drakon.SkewerBlock where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..), (#), p2, position, r2, translate)
import Drakon.Constants
import Drakon.Content
import Drakon.HelperDiagrams
import Drakon.ID
import Drakon.TypeClasses
import Drakon.ValentPoint

render' :: [SkewerBlock] -> Point V2 Double -> (Diagram B, Double)
render' skewerBlocks (P (V2 x y)) =
  foldl
    (\accu singleBlock ->
       let diagram = fst accu
           preY1 = snd accu
           connectionX = x + defaultBoundingBoxWidth * 0.5
           preY2 = preY1 - defaultBoundingBoxHeight * 0.25
           postY1 = preY2 - defaultBoundingBoxHeight * 0.5
           postY2 = preY1 - defaultBoundingBoxHeight
        in ( renderedConnection [p2 (connectionX, preY1), p2 (connectionX, preY2)]
               <> diagram
               <> renderedConnection [p2 (connectionX, postY1), p2 (connectionX, postY2)]
               <> render singleBlock (P (V2 x (snd accu)))
           , snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
    (mempty, y)
    skewerBlocks

widthInUnits' :: [SkewerBlock] -> Double
widthInUnits' skewerBlocks = maximum $ map widthInUnits skewerBlocks

heightInUnits' :: [SkewerBlock] -> Double
heightInUnits' skewerBlocks = sum $ map heightInUnits skewerBlocks

data SkewerBlock
  = Action ID Content
  | Question String
  | Fork ID Content [SkewerBlock] [SkewerBlock]

instance Show SkewerBlock where
  show (Action (ID id) (Content content)) = "[" <> id <> "] " <> content
  show _ = ""

instance Renderer SkewerBlock where
  render action@(Action (ID actionId) (Content actionContent)) origin =
    let iconHeight = heightInUnits action * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                ((if troubleshootingMode
                    then "[" <> actionId <> "] "
                    else "")
                   <> actionContent)
                (0.0 + widthInUnits action * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits action * defaultBoundingBoxHeight * 0.5)
                <> rect' (widthInUnits action * defaultBoundingBoxWidth * widthRatio) iconHeight
                     # translate (r2 (defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                     then boundingBox
                            (widthInUnits action * defaultBoundingBoxWidth)
                            (heightInUnits action * defaultBoundingBoxHeight)
                     else mempty)
          ]
  render question@(Question content) origin =
    let iconHeight = heightInUnits question * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                content
                (0.0 + widthInUnits question * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits question * defaultBoundingBoxHeight * 0.5)
                <> hex' (widthInUnits question * defaultBoundingBoxWidth * widthRatio) iconHeight
                     # translate (r2 (0.1 + defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                     then boundingBox
                            (widthInUnits question * defaultBoundingBoxWidth)
                            (heightInUnits question * defaultBoundingBoxHeight)
                     else mempty)
          ]
  render fork@(Fork (ID _) (Content content) l r) origin@(P (V2 x y)) =
    let question = Question content
        lOrigin = P (V2 x (y - heightInUnits question * defaultBoundingBoxHeight))
        rOrigin@(P (V2 rX rY)) =
          P (V2 (x + widthInUnits' l * defaultBoundingBoxWidth) (y - heightInUnits question * defaultBoundingBoxHeight))
        connectionLX = x + defaultBoundingBoxWidth * 0.5
     in render question origin
          <> if null l
               then render ValentPoint lOrigin
               else renderText
                      "no"
                      (x + widthInUnits question * defaultBoundingBoxWidth * 0.97)
                      (y - heightInUnits question * defaultBoundingBoxHeight * 0.35)
                      <> renderText
                           "yes"
                           (x + widthInUnits question * defaultBoundingBoxWidth * 0.42)
                           (y - heightInUnits question * defaultBoundingBoxHeight * 0.9)
                      <> fst (render' l lOrigin)
                      <> renderedConnection
                           [ p2 (connectionLX, y - heightInUnits' l * defaultBoundingBoxHeight)
                           , p2 (connectionLX, y - heightInUnits fork * defaultBoundingBoxHeight)
                           ]
                      <> renderedConnection
                           [ p2
                               ( x + widthInUnits question * defaultBoundingBoxWidth * (widthRatio + 1) / 2.0
                               , y - heightInUnits question * defaultBoundingBoxHeight * 0.5)
                           , p2
                               ( rX + defaultBoundingBoxWidth * 0.5
                               , y - heightInUnits question * defaultBoundingBoxHeight * 0.5)
                           , p2 (rX + defaultBoundingBoxWidth * 0.5, rY - defaultBoundingBoxHeight * 0.25)
                           ]
                      <> if null r
                           then render ValentPoint rOrigin
                                  <> renderedConnection
                                       [ p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits ValentPoint * defaultBoundingBoxHeight)
                                       , p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       , p2
                                           ( x + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       ]
                           else fst (render' r rOrigin)
                                  <> position
                                       [ ( origin
                                         , if troubleshootingMode
                                             then boundingBox
                                                    (widthInUnits fork * defaultBoundingBoxWidth)
                                                    (heightInUnits fork * defaultBoundingBoxHeight)
                                             else mempty)
                                       ]
                                  <> renderedConnection
                                       [ p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits' r * defaultBoundingBoxHeight)
                                       , p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       , p2
                                           ( x + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       ]
  widthInUnits (Action _ _) = 1.0
  widthInUnits (Question _) = 1.0
  widthInUnits (Fork _ _ l r) =
    (if null l
       then widthInUnits ValentPoint
       else widthInUnits' l)
      + (if null r
           then widthInUnits ValentPoint
           else widthInUnits' r)
  heightInUnits (Action _ _) = 1.0
  heightInUnits (Question _) = 1.0
  heightInUnits (Fork _ (Content content) l r) =
    let question = Question content
     in heightInUnits question
          + max
              (if null l
                 then heightInUnits ValentPoint
                 else heightInUnits' l)
              (if null r
                 then heightInUnits ValentPoint
                 else heightInUnits' r)
