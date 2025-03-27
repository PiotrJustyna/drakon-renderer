module Drakon.TypeClasses where

import Data.Map (Map)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..))
import Drakon.ID (ID)

class Renderer a where
  render :: a -> Map ID (Point V2 Double) -> Diagram B
  widthInUnits :: a -> Double
  heightInUnits :: a -> Double
