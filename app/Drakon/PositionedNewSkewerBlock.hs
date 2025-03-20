module Drakon.PositionedNewSkewerBlock where

import Diagrams.Prelude (Point(..), V2(..))
import Drakon.ConnectedSkewerBlocks (ConnectedSkewerBlocks(..))
import Drakon.NewSkewerBlock (NewSkewerBlock(NewAction, NewFork), heightInUnits)
import Drakon.Constants (defaultBoundingBoxHeight)
import Drakon.Content (Content)
import Drakon.ID (ID)

data PositionedNewSkewerBlock
  = PositionedNewAction ID Content (Point V2 Double)
  | PositionedNewFork
      ID
      Content
      (Point V2 Double)
      (ConnectedSkewerBlocks PositionedNewSkewerBlock)
      (ConnectedSkewerBlocks PositionedNewSkewerBlock)
  deriving (Show)

toPositionedNewSkewerBlock :: NewSkewerBlock -> Point V2 Double -> PositionedNewSkewerBlock
toPositionedNewSkewerBlock (NewAction id content) origin = PositionedNewAction id content origin
toPositionedNewSkewerBlock (NewFork id content leftBranch rightBranch) origin =
  PositionedNewFork id content origin (ConnectedSkewerBlocks [] Nothing) (ConnectedSkewerBlocks [] Nothing)
  -- TODO 1:
  -- (toPositionedConnectedSkewerBlocks leftBranch) (toPositionedConnectedSkewerBlocks rightBranch)

append :: PositionedNewSkewerBlock -> ConnectedSkewerBlocks PositionedNewSkewerBlock -> ConnectedSkewerBlocks PositionedNewSkewerBlock
append x (ConnectedSkewerBlocks y id) = ConnectedSkewerBlocks (y <> [x]) id

positionSkewerBlocks :: ConnectedSkewerBlocks NewSkewerBlock -> Point V2 Double -> ConnectedSkewerBlocks PositionedNewSkewerBlock
positionSkewerBlocks (ConnectedSkewerBlocks skewerBlocks id) (P (V2 x y)) =
  fst
    $ foldl
        (\accu singleBlock ->
           let positionedSkewerBlocks = fst accu
               newPositionedSkewerBlock = toPositionedNewSkewerBlock singleBlock (P (V2 x (snd accu)))
            in (append newPositionedSkewerBlock positionedSkewerBlocks, snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
        (ConnectedSkewerBlocks [] id, y)
        skewerBlocks
