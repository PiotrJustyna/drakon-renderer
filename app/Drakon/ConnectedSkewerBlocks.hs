module Drakon.ConnectedSkewerBlocks where

import Drakon.ID (ID)

data ConnectedSkewerBlocks a =
  ConnectedSkewerBlocks [a] (Maybe ID)
  deriving (Show)
