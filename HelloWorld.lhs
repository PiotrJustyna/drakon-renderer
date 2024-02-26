> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> {-# LANGUAGE TypeFamilies              #-}
> 
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> 
> stepsOrigins :: [Point V2 Double]
> stepsOrigins = map p2 $ [(x, y) | x <- [0..1], y <- [0, (-1), (-2)]]
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
> numberOfSteps :: Int
> numberOfSteps = length stepsOrigins
>
> main =
>   mainWith $
>   atPoints stepsOrigins [step x | x <- [0 .. ]]
>   # applyAll [lw veryThin . connectOutside x (x + 1) | x <- [0 .. numberOfSteps - 2]]
