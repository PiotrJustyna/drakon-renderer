module Drakon.SkewerBlock where

import Data.Map (Map, empty, insert)
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
               <> render singleBlock
           , snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
    (mempty, y)
    skewerBlocks

renderIcons :: [SkewerBlock] -> Diagram B
renderIcons =
  foldl
    (\accu singleBlock ->
       let (P (V2 x preY1)) = getOrigin singleBlock
           connectionX = x + defaultBoundingBoxWidth * 0.5
           preY2 = preY1 - defaultBoundingBoxHeight * 0.25
           postY1 = preY2 - defaultBoundingBoxHeight * 0.5
           postY2 = preY1 - defaultBoundingBoxHeight
        in renderedConnection [p2 (connectionX, preY1), p2 (connectionX, preY2)]
             <> accu
             <> renderedConnection [p2 (connectionX, postY1), p2 (connectionX, postY2)]
             <> render singleBlock)
    mempty

position' :: [SkewerBlock] -> Point V2 Double -> [SkewerBlock]
position' skewerBlocks (P (V2 x y)) =
  fst
    $ foldl
        (\accu singleBlock ->
           let positionedSkewerBlocks = fst accu
            in ( positionedSkewerBlocks <> [changeOrigin singleBlock (P (V2 x (snd accu)))]
               , snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
        ([], y)
        skewerBlocks

widthInUnits' :: [SkewerBlock] -> Double
widthInUnits' skewerBlocks = maximum $ map widthInUnits skewerBlocks

heightInUnits' :: [SkewerBlock] -> Double
heightInUnits' skewerBlocks = sum $ map heightInUnits skewerBlocks

toMap :: [SkewerBlock] -> Map ID (Point V2 Double)
toMap = foldl (flip insertToMap) empty

-- #1 - possible state
-- Q - +
-- |   |
-- L   R
-- |   |
-- A1  A3
-- | - +
-- A2
-- |
-- A3
-- |
-- END
-- #2 - possible state
-- Q - +
-- |   |
-- L   R
-- |   |
-- A1  |
-- |   |
-- A2  |
-- |   |
-- A3  |
-- | - +
-- END
-- #3 - impossible state
-- | - - - +
-- A0      |
-- |       |
-- Q - +   |
-- |   |   |
-- L   R   |
-- |   |   |
-- A1  A3  |
-- |   |   |
-- A2  A4  |
-- |   |   |
-- A3  + - +
-- |
-- END
data ConnectedSkewerBlocks =
  ConnectedSkewerBlocks [SkewerBlock] (Maybe ID)
  deriving (Show)

data SkewerBlock
  = Action ID (Point V2 Double) Content
  | Question ID (Point V2 Double) Content
  | Fork ID (Point V2 Double) Content ConnectedSkewerBlocks ConnectedSkewerBlocks

getId :: SkewerBlock -> ID
getId (Action actionId _ _) = actionId
getId (Question questionId _ _) = questionId
getId (Fork forkId _ _ _ _) = forkId

getOrigin :: SkewerBlock -> Point V2 Double
getOrigin (Action _ origin _) = origin
getOrigin (Question _ origin _) = origin
getOrigin (Fork _ origin _ _ _) = origin

insertToMap :: SkewerBlock -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
insertToMap skewerBlock@(Action actionId _ _) startingMap = insert actionId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Question questionId _ _) startingMap = insert questionId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Fork forkId _ _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) startingMap =
  let leftMap = toMap l
      rightMap = toMap r
   in insert forkId (getOrigin skewerBlock) (startingMap <> leftMap <> rightMap)

changeOrigin :: SkewerBlock -> Point V2 Double -> SkewerBlock
changeOrigin (Action actionId _ content) newOrigin = Action actionId newOrigin content
changeOrigin (Question questionId _ content) newOrigin = Question questionId newOrigin content
changeOrigin (Fork forkId _ content (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) newOrigin@(P (V2 x y)) =
  let question = Question forkId newOrigin content
      lOrigin = P (V2 x (y - heightInUnits question * defaultBoundingBoxHeight))
      rOrigin =
        P (V2 (x + widthInUnits' l * defaultBoundingBoxWidth) (y - heightInUnits question * defaultBoundingBoxHeight))
      newL = ConnectedSkewerBlocks (position' l lOrigin) Nothing
      newR = ConnectedSkewerBlocks (position' r rOrigin) Nothing
   in Fork forkId newOrigin content newL newR

instance Show SkewerBlock where
  show (Action (ID actionId) origin (Content content)) =
    "[ID: " <> actionId <> " | Origin: " <> show origin <> "] " <> content
  show (Fork (ID forkId) origin (Content content) _ _) =
    "[ID: " <> forkId <> " | Origin: " <> show origin <> "] " <> content
  show _ = ""

instance Renderer SkewerBlock where
  render action@(Action actionId origin (Content actionContent)) =
    let iconHeight = heightInUnits action * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                ((if troubleshootingMode
                    then "[" <> show actionId <> " | " <> show origin <> "] "
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
  render question@(Question questionId origin (Content content)) =
    let iconHeight = heightInUnits question * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                ((if troubleshootingMode
                    then "[" <> show questionId <> " | " <> show origin <> "] "
                    else "")
                   <> content)
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
  render fork@(Fork forkId origin@(P (V2 x y)) content (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) =
    let question = Question forkId origin content
        lOrigin = P (V2 x (y - heightInUnits question * defaultBoundingBoxHeight))
        rOrigin@(P (V2 rX rY)) =
          P (V2 (x + widthInUnits' l * defaultBoundingBoxWidth) (y - heightInUnits question * defaultBoundingBoxHeight))
        connectionLX = x + defaultBoundingBoxWidth * 0.5
     in render question
          <> if null l
               then render (ValentPoint lOrigin)
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
                           then render (ValentPoint rOrigin)
                                  <> renderedConnection
                                       [ p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y
                                               - heightInUnits (ValentPoint (p2 (-1.0, -1.0)))
                                                   * defaultBoundingBoxHeight)
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
  widthInUnits (Action {}) = 1.0
  widthInUnits (Question {}) = 1.0
  widthInUnits (Fork _ _ _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) =
    (if null l
       then widthInUnits (ValentPoint (p2 (-1.0, -1.0)))
       else widthInUnits' l)
      + (if null r
           then widthInUnits (ValentPoint (p2 (-1.0, -1.0)))
           else widthInUnits' r)
  heightInUnits (Action {}) = 1.0
  heightInUnits (Question {}) = 1.0
  heightInUnits (Fork _questionId _origin (Content content) (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) =
    heightInUnits (Question _questionId _origin (Content content))
      + max
          (if null l
             then heightInUnits (ValentPoint (p2 (-1.0, -1.0)))
             else heightInUnits' l)
          (if null r
             then heightInUnits (ValentPoint (p2 (-1.0, -1.0)))
             else heightInUnits' r)
