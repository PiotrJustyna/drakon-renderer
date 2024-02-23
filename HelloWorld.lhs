> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> {-# LANGUAGE TypeFamilies              #-}
> 
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> 
> myCircle :: Diagram B
> myCircle = circle 1
> 
> example :: Diagram B
> example = circle 1 # fc blue
>
> twoCircles :: Diagram B
> twoCircles = circle 1 # fc green ||| circle 1 # fc red
>
> twoShapes :: Diagram B
> twoShapes = square 1 # fc aqua `atop` circle 1
>
> main = mainWith twoCircles