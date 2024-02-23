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
> vectorShape :: Diagram B
> vectorShape = fromOffsets . map r2 $ [(0,0), (1,1), (-2,1), (-1,-4)]
> 
> step :: Diagram B
> step = fromOffsets
>   [V2 0 0,
>   V2 0 0.25,
>   V2 0.5 0,
>   V2 0 (-0.25),
>   V2 (-0.5) 0]
>   # lw veryThin
>   # showOrigin
>
> drakonPoints :: [Point V2 Double]
> drakonPoints = map p2 $ [(x, y) | x <- [0..5], y <- [0..5]]
>
> main = mainWith $ atPoints drakonPoints (repeat step)