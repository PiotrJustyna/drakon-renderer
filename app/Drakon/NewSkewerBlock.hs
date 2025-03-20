module Drakon.NewSkewerBlock where

import Drakon.ConnectedSkewerBlocks (ConnectedSkewerBlocks)
import Drakon.Content (Content)
import Drakon.ID (ID)

data NewSkewerBlock
  = NewAction ID Content
  | NewFork ID Content (ConnectedSkewerBlocks NewSkewerBlock) (ConnectedSkewerBlocks NewSkewerBlock)
  deriving (Show)

-- TODO 2:
heightInUnits :: NewSkewerBlock -> Double
heightInUnits (NewAction _ _) = 1.0
