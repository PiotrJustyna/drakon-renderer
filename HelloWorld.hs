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
    Start
    | End
    | Command
    | Decision

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

steps :: Tree Step
steps =
    Node1
        (Main.Start)
        (Node2
            (Leaf Main.Command)
            Main.Decision
            (Leaf Main.End))

flattenSteps :: Tree Step -> Double -> Double -> [(OriginCoordinates, Diagram B)]
flattenSteps (Leaf x) currentWidth currentDepth =
    [(p2 (currentWidth, currentDepth), (Main.render x currentWidth currentDepth))]
flattenSteps (Node1 x y) currentWidth currentDepth =
    [(p2 (currentWidth, currentDepth), (Main.render x currentWidth currentDepth))]
    ++ flattenSteps y currentWidth (currentDepth - 1.0)
flattenSteps (Node2 x y z) currentWidth currentDepth =
    flattenSteps x currentWidth (currentDepth - 1.0)
    ++ [(p2 (currentWidth, currentDepth), (Main.render y currentWidth currentDepth))]
    ++ flattenSteps z (currentWidth + 1) (currentDepth - 1.0)

render :: Step -> Double -> Double -> Diagram B
render Main.Start x y = startShape $ uniqueName x y
render Main.End x y = endShape $ uniqueName x y
render Main.Decision x y = decisionShape $ uniqueName x y
render Main.Command x y = stepShape $ uniqueName x y

main = mainWith $
    position (flattenSteps steps 0.0 0.0)
    # lw veryThin
