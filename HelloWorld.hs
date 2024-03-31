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
startShape name = text (name ++ ": start") # fontSize (local 0.1) # light # font "courier" <>
    roundedRect stepWidth stepHeight 0.5 # showOrigin # named name

endShape :: Main.Name -> Double -> Double -> Diagram B
endShape name x y = text (name ++ ": end") # fontSize (local 0.1) # thinWeight # font "courier" <>
    roundedRect stepWidth stepHeight 0.5 # showOrigin # named name <>
    fromOffsets [V2 0 y] <>
    fromOffsets [V2 x 0] # translate (r2 (0, y))

commandShape :: Main.Name -> Double -> Double -> Diagram B
commandShape name x y = text name # fontSize (local 0.1) # light # font "courier" <>
    rect stepWidth stepHeight # showOrigin # named name <>
    fromOffsets [V2 0 y] <>
    fromOffsets [V2 x 0] # translate (r2 (0, y))

decisionShape :: Main.Name -> Double -> Double -> Diagram B
decisionShape name x y = text name # fontSize (local 0.1) # light # font "courier" <>
    fromOffsets
    [V2 (-0.1) (stepHeight * 0.5),
    V2 0.1 (stepHeight * 0.5),
    V2 (stepWidth - 0.1 - 0.1) 0.0,
    V2 0.1 (stepHeight * (-0.5)),
    V2 (-0.1) (stepHeight * (-0.5)),
    V2 ((stepWidth - 0.1 - 0.1) * (-1.0)) 0.0] # translate (r2 (((stepWidth - 0.1 - 0.1) * (-0.5)), (-0.2))) # showOrigin # named name <>
    fromOffsets [V2 0 y] <>
    fromOffsets [V2 x 0] # translate (r2 (0, y)) <>
    text "yes" # fontSize (local 0.1) # light # font "courier" # translate (r2 (stepWidth * (-0.1), stepHeight * (-0.7))) <>
    text "no" # fontSize (local 0.1) # light # font "courier" # translate (r2 (stepWidth * 0.6, stepHeight * 0.15))

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

uniqueCoordinates :: Tree Step -> Double -> Double -> Double -> Double -> [(OriginCoordinates, Diagram B)] -> [(OriginCoordinates, Diagram B)]
uniqueCoordinates (Leaf x) currentWidth currentDepth previousStepOriginCoordinateX previousStepOriginCoordinateY takenCoordinates =
    [(newCoordinates, diagram)]
    where
        newDepth = nextAvailableCoordinates currentWidth currentDepth takenCoordinates
        newCoordinates = p2 (currentWidth, newDepth)
        diagram = Main.render x currentWidth newDepth (previousStepOriginCoordinateX - currentWidth) (previousStepOriginCoordinateY - newDepth)
uniqueCoordinates (Node1 x y) currentWidth currentDepth previousStepOriginCoordinateX previousStepOriginCoordinateY takenCoordinates =
    [(newCoordinates, diagram)]
    ++ subTreeCoordinates
    where
        newDepth = nextAvailableCoordinates currentWidth currentDepth takenCoordinates
        newCoordinates = p2 (currentWidth, newDepth)
        diagram = Main.render x currentWidth newDepth (previousStepOriginCoordinateX - currentWidth) (previousStepOriginCoordinateY - newDepth)
        subTreeCoordinates = uniqueCoordinates y currentWidth (newDepth - cellHeight) currentWidth newDepth ((newCoordinates, diagram) : takenCoordinates)
uniqueCoordinates (Node2 x y z) currentWidth currentDepth previousStepOriginCoordinateX previousStepOriginCoordinateY takenCoordinates =
    [(newCoordinates, diagram)]
    ++ right
    ++ uniqueCoordinates x currentWidth (newDepth - cellHeight) currentWidth newDepth (right ++ takenCoordinates)
    where
        newDepth = nextAvailableCoordinatesForBranchingStep currentWidth currentDepth takenCoordinates
        newCoordinates = p2 (currentWidth, newDepth)
        right = uniqueCoordinates z (currentWidth + cellWidth) (newDepth - cellHeight) currentWidth newDepth ((newCoordinates, diagram) : takenCoordinates)
        diagram = Main.render y currentWidth newDepth (previousStepOriginCoordinateX - currentWidth) (previousStepOriginCoordinateY - newDepth)

render :: Step -> Double -> Double -> Double -> Double -> Diagram B
render Main.Start x1 y1 x2 y2 = startShape $ uniqueName x1 y1
render Main.End x1 y1 x2 y2 = endShape (uniqueName x1 y1) x2 y2
render Main.Decision x1 y1 x2 y2 = decisionShape (uniqueName x1 y1) x2 y2
render Main.Command x1 y1 x2 y2 = commandShape (uniqueName x1 y1) x2 y2

main = mainWith $
    position (uniqueCoordinates steps 0.0 0.0 0.0 0.0 [])
    # lw veryThin
