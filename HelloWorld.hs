{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text

data Tree a =
    Leaf a
    | Node1 a (Tree a)
    | Node2 (Tree a) a (Tree a)

type StepName = Double

type OriginCoordinates = Point V2 Double

data Step =
    Start StepName OriginCoordinates
    | End StepName OriginCoordinates
    | Command StepName OriginCoordinates
    | Decision StepName OriginCoordinates

stepName :: Step -> Double
stepName (Main.Start x _) = x
stepName (Main.End x _) = x
stepName (Main.Command x _) = x
stepName (Main.Decision x _) = x

stepOriginCoordinates :: Step -> OriginCoordinates
stepOriginCoordinates (Main.Start _ x) = x
stepOriginCoordinates (Main.End _ x) = x
stepOriginCoordinates (Main.Command _ x) = x
stepOriginCoordinates (Main.Decision _ x) = x

stepShape :: Double -> Diagram B
stepShape x = rect 0.95 0.4 # showOrigin # named x

startShape :: Double -> Diagram B
startShape x = text "start" # fontSize (local 0.1) # light # font "courier" <> roundedRect 1.0 0.4 0.5 # showOrigin # named x

endShape :: Double -> Diagram B
endShape x = text "end" # fontSize (local 0.1) # thinWeight # font "courier" <> roundedRect 1.0 0.4 0.5 # showOrigin # named x

decisionShape :: Double -> Diagram B
decisionShape x = fromOffsets
    [V2 (-0.1) 0.2,
    V2 0.1 0.2,
    V2 0.8 0.0,
    V2 0.1 (-0.2),
    V2 (-0.1) (-0.2),
    V2 (-0.8) (0.0)]
    # translate (r2 ((-0.4), (-0.2)))
    # showOrigin
    # named x

uniqueName :: Double -> Double -> Double
uniqueName x y = x * 10 + (abs y)

newSteps :: Tree Step
newSteps =
    Node1
        (Main.Start (uniqueName 0.0 0.0) (p2 (0.0, 0.0)))
        (Node1
            (Main.Command (uniqueName 0.0 (-1.0)) (p2 (0.0, -1.0)))
            (Leaf (Main.End (uniqueName 0.0 (-2.0)) (p2 (0.0, -2.0)))))

flattenSteps :: Tree Step -> [Step]
flattenSteps (Leaf x) = [x]
flattenSteps (Node1 x y) = [x] ++ flattenSteps y
flattenSteps (Node2 x y z) = flattenSteps x ++ [y] ++ flattenSteps z

correctShape :: Step -> Diagram B
correctShape (Main.Start x _) = startShape x
correctShape (Main.End x _) = endShape x
correctShape (Main.Decision x _) = decisionShape x
correctShape (Main.Command x _) = stepShape x

main = mainWith $
    position [(stepOriginCoordinates x, correctShape x) | x <- flattenSteps newSteps]
    # lw veryThin
