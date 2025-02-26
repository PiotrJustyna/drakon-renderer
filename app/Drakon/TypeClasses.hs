module Drakon.TypeClasses where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point(..), V2(..))

class Renderer a where
  render :: a -> Point V2 Double -> Diagram B
  widthInUnits :: a -> Double
  heightInUnits :: a -> Double
