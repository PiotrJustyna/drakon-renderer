module Drakon.SkewerBlock where

import Data.Map (Map, empty, insert, lookup)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..), (#), p2, position, r2, rotateBy, translate, triangle)
import Drakon.Constants
import Drakon.Content
import Drakon.HelperDiagrams
import Drakon.ID
import Drakon.TypeClasses

renderAdditionalConnection :: Point V2 Double -> ID -> Map ID (Point V2 Double) -> Diagram B
renderAdditionalConnection sourceOrigin@(P (V2 x1 y1)) destinationId mapOfOrigins =
  case Data.Map.lookup destinationId mapOfOrigins of
    (Just _destinationOrigin@(P (V2 x2 y2))) ->
      if x1 > x2 && y1 < y2
        then renderedConnection
               [ sourceOrigin
               , p2 (x1 + defaultBoundingBoxWidth * 0.5 + 0.1, y1)
               , p2 (x1 + defaultBoundingBoxWidth * 0.5 + 0.1, y2 - 0.1)
               , p2 (x2 + defaultBoundingBoxWidth * 0.5 + 0.087, y2 - 0.1)
               ]
               <> position
                    [ ( p2 (x2 + defaultBoundingBoxWidth * 0.5 + (0.087 / 2.0) + 0.02, y2 - 0.1)
                      , rotateBy (1 / 4) $ triangle 0.1 # drakonStyle)
                    ]
        else (if (x1 < x2 && y1 > y2) || (x1 > x2 && y1 > y2)
                then renderedConnection
                       [ sourceOrigin
                       , p2 (x2 + defaultBoundingBoxWidth - 0.1, y1)
                       , p2 (x2 + defaultBoundingBoxWidth - 0.1, y2 + 0.1)
                       , p2 (x2 + defaultBoundingBoxWidth * 0.5, y2 + 0.1)
                       ]
                else renderedConnection [sourceOrigin, _destinationOrigin])
               -- 0.087:   from Pythegorean theorem
               -- 0.02:  from line width?
    _ -> mempty

render' :: ConnectedSkewerBlocks -> Point V2 Double -> Map ID (Point V2 Double) -> (Diagram B, Double)
render' (ConnectedSkewerBlocks skewerBlocks _id) (P (V2 x y)) mapOfOrigins =
  if null skewerBlocks
    then ( case _id of
             Just destinationId ->
               renderAdditionalConnection
                 (p2 (x - defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, y + defaultBoundingBoxHeight * 0.5))
                 destinationId
                 mapOfOrigins
             Nothing -> mempty
         , y)
    else (let connectionX = x + defaultBoundingBoxWidth * 0.5
              (renderedBlocks, lastY) =
                foldl
                  (\accu singleBlock ->
                     let diagram = fst accu
                         preY1 = snd accu
                         preY2 = preY1 - defaultBoundingBoxHeight * 0.25
                         postY1 = preY2 - defaultBoundingBoxHeight * 0.5
                         postY2 = preY1 - defaultBoundingBoxHeight
                      in ( renderedConnection [p2 (connectionX, preY1), p2 (connectionX, preY2)]
                             <> diagram
                             <> renderedConnection [p2 (connectionX, postY1), p2 (connectionX, postY2)]
                             <> render singleBlock mapOfOrigins
                         , snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
                  (mempty, y)
                  skewerBlocks
           in case _id of
                Just destinationId ->
                  ( renderedBlocks <> renderAdditionalConnection (p2 (connectionX, lastY)) destinationId mapOfOrigins
                  , lastY)
                _ -> (renderedBlocks, lastY))

renderIcons :: [SkewerBlock] -> Map ID (Point V2 Double) -> Diagram B
renderIcons skewerBlocks mapOfOrigins =
  foldl
    (\accu singleBlock ->
       let (P (V2 x preY1)) = getOrigin singleBlock
           connectionX = x + defaultBoundingBoxWidth * 0.5
           preY2 = preY1 - defaultBoundingBoxHeight * 0.25
           postY1 = preY2 - defaultBoundingBoxHeight * blockHeightInUnits singleBlock
           postY2 = preY1 - defaultBoundingBoxHeight
        in renderedConnection [p2 (connectionX, preY1), p2 (connectionX, preY2)]
             <> accu
             <> renderedConnection [p2 (connectionX, postY1), p2 (connectionX, postY2)]
             <> render singleBlock mapOfOrigins)
    mempty
    skewerBlocks

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
widthInUnits' x =
  case x of
    [] -> 1.0
    skewerBlocks -> maximum $ map widthInUnits skewerBlocks

heightInUnits' :: [SkewerBlock] -> Double
heightInUnits' skewerBlocks = sum $ map heightInUnits skewerBlocks

toMap :: [SkewerBlock] -> Map ID (Point V2 Double)
toMap = foldl (flip insertToMap) empty

data ConnectedSkewerBlocks =
  ConnectedSkewerBlocks [SkewerBlock] (Maybe ID)
  deriving (Show)

data SkewerBlock
  = Action ID (Point V2 Double) Content
  | Headline ID (Point V2 Double) Content
  | Address ID (Point V2 Double) Content
  | Fork ID (Point V2 Double) Content ConnectedSkewerBlocks ConnectedSkewerBlocks

getOrigin :: SkewerBlock -> Point V2 Double
getOrigin (Action _ origin _) = origin
getOrigin (Headline _ origin _) = origin
getOrigin (Address _ origin _) = origin
getOrigin (Fork _ origin _ _ _) = origin

insertToMap :: SkewerBlock -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
insertToMap skewerBlock@(Action actionId _ _) startingMap = insert actionId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Headline headlineId _ _) startingMap = insert headlineId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Address addressId _ _) startingMap = insert addressId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Fork forkId _ _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) startingMap =
  let leftMap = toMap l
      rightMap = toMap r
   in insert forkId (getOrigin skewerBlock) (startingMap <> leftMap <> rightMap)

changeOrigin :: SkewerBlock -> Point V2 Double -> SkewerBlock
changeOrigin (Action actionId _ content) newOrigin = Action actionId newOrigin content
changeOrigin (Headline headlineId _ content) newOrigin = Headline headlineId newOrigin content
changeOrigin (Address addressId _ content) newOrigin = Address addressId newOrigin content
changeOrigin (Fork forkId _ content (ConnectedSkewerBlocks l leftId) (ConnectedSkewerBlocks r rightId)) newOrigin@(P (V2 x y)) =
  let lOrigin = P (V2 x (y - defaultBoundingBoxHeight))
      rOrigin = P (V2 (x + widthInUnits' l * defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight))
      newL = ConnectedSkewerBlocks (position' l lOrigin) leftId
      newR = ConnectedSkewerBlocks (position' r rOrigin) rightId
   in Fork forkId newOrigin content newL newR

instance Show SkewerBlock where
  show (Action (ID actionId) origin (Content content)) =
    "[ID: " <> actionId <> " | Origin: " <> show origin <> "] " <> content
  show (Headline (ID headlineId) origin (Content content)) =
    "[ID: " <> headlineId <> " | Origin: " <> show origin <> "] " <> content
  show (Address (ID addressId) origin (Content content)) =
    "[ID: " <> addressId <> " | Origin: " <> show origin <> "] " <> content
  show (Fork (ID forkId) origin (Content content) _ _) =
    "[ID: " <> forkId <> " | Origin: " <> show origin <> "] " <> content

renderQuestion :: ID -> Point V2 Double -> Content -> Map ID (Point V2 Double) -> Diagram B
renderQuestion questionId origin (Content content) _mapOfOrigins =
  position
    [ ( origin
      , renderText
          ((if troubleshootingMode
              then "[" <> show questionId <> " | " <> show origin <> "] "
              else "")
             <> content)
          (0.0 + defaultBoundingBoxWidth * 0.5)
          (0.0 - defaultBoundingBoxHeight * 0.5)
          <> hex' (defaultBoundingBoxWidth * widthRatio) (defaultBoundingBoxHeight * 0.5)
               # translate
                   (r2 (0.1 + defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, defaultBoundingBoxHeight * (-0.25)))
          <> if troubleshootingMode
               then boundingBox defaultBoundingBoxWidth defaultBoundingBoxHeight
               else mempty)
    ]

instance Renderer SkewerBlock where
  render action@(Action actionId origin (Content actionContent)) _mapOfOrigins =
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
  render headline@(Headline headlineId origin (Content headlineContent)) _mapOfOrigins =
    let iconHeight = heightInUnits headline * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                ((if troubleshootingMode
                    then "[" <> show headlineId <> " | " <> show origin <> "] "
                    else "")
                   <> headlineContent)
                (0.0 + widthInUnits headline * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits headline * defaultBoundingBoxHeight * 0.5)
                <> headlineShape (widthInUnits headline * defaultBoundingBoxWidth * widthRatio) iconHeight
                     # translate (r2 (defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                     then boundingBox
                            (widthInUnits headline * defaultBoundingBoxWidth)
                            (heightInUnits headline * defaultBoundingBoxHeight)
                     else mempty)
          ]
  render address@(Address addressId origin (Content addressContent)) _mapOfOrigins =
    let iconHeight = heightInUnits address * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                ((if troubleshootingMode
                    then "[" <> show addressId <> " | " <> show origin <> "] "
                    else "")
                   <> addressContent)
                (0.0 + widthInUnits address * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits address * defaultBoundingBoxHeight * 0.5)
                <> rect' (widthInUnits address * defaultBoundingBoxWidth * widthRatio) iconHeight
                     # translate (r2 (defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                     then boundingBox
                            (widthInUnits address * defaultBoundingBoxWidth)
                            (heightInUnits address * defaultBoundingBoxHeight)
                     else mempty)
          ]
  render fork@(Fork forkId origin@(P (V2 x y)) content leftBranch@(ConnectedSkewerBlocks l lDetourId) rightBranch@(ConnectedSkewerBlocks r rDetourId)) _mapOfOrigins =
    let lOrigin@(P (V2 _ lY)) = P (V2 x (y - defaultBoundingBoxHeight))
        rOrigin@(P (V2 rX rY)) = P (V2 (x + widthInUnits' l * defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight))
        connectionLX = x + defaultBoundingBoxWidth * 0.5
     in renderQuestion forkId origin content _mapOfOrigins
          <> fst (render' leftBranch lOrigin _mapOfOrigins)
          <> renderText "no" (x + defaultBoundingBoxWidth * 0.97) (y - defaultBoundingBoxHeight * 0.35)
          <> renderText "yes" (x + defaultBoundingBoxWidth * 0.42) (y - defaultBoundingBoxHeight * 0.9)
          <> case lDetourId of
               Nothing ->
                 renderedConnection
                   [ p2 (connectionLX, lY - heightInUnits' l * defaultBoundingBoxHeight)
                   , p2 (connectionLX, y - heightInUnits fork * defaultBoundingBoxHeight)
                   ]
               Just _ -> mempty
          <> (if null r
                then (case rDetourId of
                        Nothing ->
                          renderedConnection
                            [ p2
                                ( x + defaultBoundingBoxWidth * (widthRatio + 1) / 2.0
                                , y - defaultBoundingBoxHeight * 0.5)
                            , p2 (rX - 0.1, y - defaultBoundingBoxHeight * 0.5)
                            , p2 (rX - 0.1, rY - defaultBoundingBoxHeight * 0.25)
                            ]
                        Just _ -> fst (render' rightBranch rOrigin _mapOfOrigins))
                else renderedConnection
                       [ p2 (x + defaultBoundingBoxWidth * (widthRatio + 1) / 2.0, y - defaultBoundingBoxHeight * 0.5)
                       , p2 (rX + defaultBoundingBoxWidth * 0.5, y - defaultBoundingBoxHeight * 0.5)
                       , p2 (rX + defaultBoundingBoxWidth * 0.5, rY - defaultBoundingBoxHeight * 0.25)
                       ]
                       <> fst (render' rightBranch rOrigin _mapOfOrigins))
          <> position
               [ ( origin
                 , if troubleshootingMode
                     then boundingBox
                            (widthInUnits fork * defaultBoundingBoxWidth)
                            (heightInUnits fork * defaultBoundingBoxHeight)
                     else mempty)
               ]
          <> case rDetourId of
               Nothing ->
                 (if null r
                    then renderedConnection
                           [ p2 (rX - 0.1, y - defaultBoundingBoxHeight * 1.25)
                           , p2 (rX - 0.1, y - heightInUnits fork * defaultBoundingBoxHeight)
                           , p2 (x + defaultBoundingBoxWidth * 0.5, y - heightInUnits fork * defaultBoundingBoxHeight)
                           ]
                    else renderedConnection
                           [ p2 (rX + defaultBoundingBoxWidth * 0.5, y - defaultBoundingBoxHeight)
                           , p2 (rX + defaultBoundingBoxWidth * 0.5, y - heightInUnits fork * defaultBoundingBoxHeight)
                           , p2 (x + defaultBoundingBoxWidth * 0.5, y - heightInUnits fork * defaultBoundingBoxHeight)
                           ])
               Just _ -> mempty
  widthInUnits (Fork _ _ _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) =
    (if null l
       then 1.0
       else widthInUnits' l)
      + (if null r
           then 0.0
           else widthInUnits' r)
  widthInUnits _ = 1.0
  heightInUnits (Fork _forkId _origin _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) =
    1.0
      + max
          (if null l
             then 0.0
             else heightInUnits' l)
          (if null r
             then 0.0
             else heightInUnits' r)
  heightInUnits Headline {} = 1.0
  heightInUnits _ = 1.0

blockHeightInUnits :: SkewerBlock -> Double
blockHeightInUnits Headline {} = 0.6
blockHeightInUnits _ = 0.5
