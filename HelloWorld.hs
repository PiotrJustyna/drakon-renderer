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

cellWidth :: Double
cellWidth = 2.0

cellHeight :: Double
cellHeight = 1.0

stepWidthToCellWidthRatio :: Double
stepWidthToCellWidthRatio = 0.8

stepHeightToCellHeightRatio :: Double
stepHeightToCellHeightRatio = 0.4

stepWidth :: Double
stepWidth = cellWidth * stepWidthToCellWidthRatio

stepHeight :: Double
stepHeight = cellHeight *stepHeightToCellHeightRatio

shortestDistanceBetweenSteps :: Double
shortestDistanceBetweenSteps = cellHeight * (1.0 - stepHeightToCellHeightRatio)

startShape :: Main.Name -> Diagram B
startShape x = text ((show x) ++ ": start") # fontSize (local 0.1) # light # font "courier" <>
    roundedRect stepWidth stepHeight 0.5 # showOrigin # named x <>
    fromOffsets [V2 0.0 (shortestDistanceBetweenSteps * (-1.0))] # translate (r2 (0.0, stepHeight * (-0.5)))

endShape :: Main.Name -> Diagram B
endShape x = text ((show x) ++ ": end") # fontSize (local 0.1) # thinWeight # font "courier" <>
    roundedRect stepWidth stepHeight 0.5 # showOrigin # named x

commandShape :: Main.Name -> Diagram B
commandShape x = text (show x) # fontSize (local 0.1) # light # font "courier" <>
    rect stepWidth stepHeight # showOrigin # named x <>
    fromOffsets [V2 0.0 (shortestDistanceBetweenSteps * (-1.0))] # translate (r2 (0.0, stepHeight * (-0.5)))

decisionShape :: Main.Name -> Diagram B
decisionShape x = text (show x) # fontSize (local 0.1) # light # font "courier" <>
        fromOffsets
        [V2 (-0.1) (stepHeight * 0.5),
        V2 0.1 (stepHeight * 0.5),
        V2 (stepWidth - 0.1 - 0.1) 0.0,
        V2 0.1 (stepHeight * (-0.5)),
        V2 (-0.1) (stepHeight * (-0.5)),
        V2 ((stepWidth - 0.1 - 0.1) * (-1.0)) 0.0] # translate (r2 (((stepWidth - 0.1 - 0.1) * (-0.5)), (-0.2))) # showOrigin # named x <>
        fromOffsets [V2 0.0 (shortestDistanceBetweenSteps * (-1.0))] # translate (r2 (0.0, stepHeight * (-0.5))) <>
        fromOffsets [V2 (cellWidth - (stepWidth * 0.5)) 0.0, V2 0.0 (shortestDistanceBetweenSteps + (stepHeight * 0.5)) * (-1.0)] # translate (r2 (stepWidth * 0.5, 0.0))

uniqueName :: Double -> Double -> Main.Name
uniqueName x y = "x" ++ (show x) ++ "y" ++ (show y)

steps :: Tree Step
steps =
    Node1
        (Main.Start)
        (Node2
            (Node1
                Main.Command
                (Node2
                    (Node1
                        Main.Command
                        (Leaf Main.End))
                    Main.Decision
                    (Node1
                        Main.Command
                        (Leaf Main.End))))
            Main.Decision
            (Node2
                (Node1
                    Main.Command
                    (Leaf Main.End))
                Main.Decision
                (Leaf Main.End)))

steps1 :: Tree Step
steps1 =
    Node1
        (Main.Start)
        (Node1
            Main.Command
                (Node2
                    (Node1
                        Main.Command
                        (Leaf Main.End))
                    Main.Decision
                    (Node1
                        Main.Command
                        (Node1
                            Main.Command
                            (Node2
                                (Leaf Main.End)
                                Main.Decision
                                (Leaf Main.End))))))

nextAvailableCoordinates :: Double -> Double -> [(OriginCoordinates, Diagram B)] -> Double
nextAvailableCoordinates x y takenCoordinates =
    if any (\(coordinates, diagram) -> (p2 (x, y)) == coordinates) takenCoordinates
    then nextAvailableCoordinates x (y - cellHeight) takenCoordinates
    else y

nextAvailableCoordinatesForBranchingStep :: Double -> Double -> [(OriginCoordinates, Diagram B)] -> Double
nextAvailableCoordinatesForBranchingStep x y takenCoordinates =
    if
        (any (\(coordinates, diagram) -> (p2 (x, y)) == coordinates) takenCoordinates)
        || (any (\(coordinates, diagram) -> (p2 (x + cellWidth, y)) == coordinates) takenCoordinates)
    then nextAvailableCoordinatesForBranchingStep x (y - cellHeight) takenCoordinates
    else y

uniqueCoordinates :: Tree Step -> Double -> Double -> [(OriginCoordinates, Diagram B)] -> [(OriginCoordinates, Diagram B)]
uniqueCoordinates (Leaf x) currentWidth currentDepth takenCoordinates =
    [(newCoordinates, Main.render x currentWidth newDepth)]
    where
        newDepth = nextAvailableCoordinates currentWidth currentDepth takenCoordinates
        newCoordinates = p2 (currentWidth, newDepth)
uniqueCoordinates (Node1 x y) currentWidth currentDepth takenCoordinates =
    [(newCoordinates, diagram)]
    ++ uniqueCoordinates y currentWidth (newDepth - cellHeight) ((newCoordinates, diagram) : takenCoordinates)
    where
        newDepth = nextAvailableCoordinates currentWidth currentDepth takenCoordinates
        newCoordinates = p2 (currentWidth, newDepth)
        diagram = Main.render x currentWidth newDepth
uniqueCoordinates (Node2 x y z) currentWidth currentDepth takenCoordinates =
    [(newCoordinates, diagram)]
    ++ right
    ++ uniqueCoordinates x currentWidth (newDepth - cellHeight) (right ++ takenCoordinates)
    where
        newDepth = nextAvailableCoordinatesForBranchingStep currentWidth currentDepth takenCoordinates
        newCoordinates = p2 (currentWidth, newDepth)
        right = uniqueCoordinates z (currentWidth + cellWidth) (newDepth - cellHeight) ((newCoordinates, diagram) : takenCoordinates)
        diagram = Main.render y currentWidth newDepth

render :: Step -> Double -> Double -> Diagram B
render Main.Start x y = startShape $ uniqueName x y
render Main.End x y = endShape $ uniqueName x y
render Main.Decision x y = decisionShape $ uniqueName x y
render Main.Command x y = commandShape $ uniqueName x y

main = mainWith $
    position (uniqueCoordinates steps 0.0 0.0 [])
    # lw veryThin
