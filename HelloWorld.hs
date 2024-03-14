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

type Name = String

type OriginCoordinates = Point V2 Double

data Step =
    Start Main.Name OriginCoordinates
    | End Main.Name OriginCoordinates
    | Command Main.Name OriginCoordinates
    | Decision Main.Name OriginCoordinates

stepName :: Step -> Main.Name
stepName (Main.Start x _) = x
stepName (Main.End x _) = x
stepName (Main.Command x _) = x
stepName (Main.Decision x _) = x

stepOriginCoordinates :: Step -> OriginCoordinates
stepOriginCoordinates (Main.Start _ x) = x
stepOriginCoordinates (Main.End _ x) = x
stepOriginCoordinates (Main.Command _ x) = x
stepOriginCoordinates (Main.Decision _ x) = x

stepShape :: Main.Name -> Diagram B
stepShape x = text (show x) # fontSize (local 0.1) # light # font "courier" <> rect 0.95 0.4 # showOrigin # named x

startShape :: Main.Name -> Diagram B
startShape x = text ((show x) ++ ": start") # fontSize (local 0.1) # light # font "courier" <> roundedRect 1.0 0.4 0.5 # showOrigin # named x

endShape :: Main.Name -> Diagram B
endShape x = text ((show x) ++ ": end") # fontSize (local 0.1) # thinWeight # font "courier" <> roundedRect 1.0 0.4 0.5 # showOrigin # named x

decisionShape :: Main.Name -> Diagram B
decisionShape x = text (show x) # fontSize (local 0.1) # light # font "courier" <>
    fromOffsets
        [V2 (-0.1) 0.2,
        V2 0.1 0.2,
        V2 0.8 0.0,
        V2 0.1 (-0.2),
        V2 (-0.1) (-0.2),
        V2 (-0.8) (0.0)]
        # translate (r2 ((-0.4), (-0.2)))
        # showOrigin
        # named x

uniqueName :: Double -> Double -> Main.Name
uniqueName x y = "x" ++ (show x) ++ "y" ++ (show y)

newSteps :: Tree Step
newSteps =
    Node1
        (Main.Start (uniqueName 0.0 0.0) (p2 (0.0, 0.0)))
        (Node2
            (Leaf (Main.Command (uniqueName 1.0 (-2.0)) (p2 (1.0, -2.0))))
            (Main.Decision (uniqueName 0.0 (-1.0)) (p2 (0.0, -1.0)))
            (Leaf (Main.End (uniqueName 0.0 (-2.0)) (p2 (0.0, -2.0)))))

newFlattenSteps :: Tree Step -> Double -> Double -> [(OriginCoordinates, Diagram B)]
newFlattenSteps (Leaf x) currentWidth currentDepth = [(p2 (currentWidth, currentDepth), (correctShape x))]
newFlattenSteps (Node1 x y) currentWidth currentDepth = [(p2 (currentWidth, currentDepth), (correctShape x))] ++ newFlattenSteps y currentWidth (currentDepth - 1.0)
newFlattenSteps (Node2 x y z) currentWidth currentDepth =  newFlattenSteps x currentWidth (currentDepth - 1.0) ++ [(p2 (currentWidth, currentDepth), (correctShape y))] ++ newFlattenSteps z (currentWidth + 1) (currentDepth - 1.0)

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
    position (newFlattenSteps newSteps 0.0 0.0)
    # lw veryThin
