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
> stepsOrigins :: [Point V2 Double]
> stepsOrigins = map p2 $ [(x, y) | x <- [0], y <- [0, (-1), (-2)]]
>
> step :: Int -> Diagram B
> step x = fromOffsets
>   [V2 0 0,
>   V2 0 0.5,
>   V2 0.75 0,
>   V2 0 (-0.5),
>   V2 (-0.75) 0]
>   # showOrigin
>   # lw veryThin
>   # named x
>
> main = mainWith $
>   atPoints stepsOrigins [step x | x <- [0..]]
>   # connectOutside (0 :: Int) (1 :: Int)
>   # connectOutside (1 :: Int) (2 :: Int)