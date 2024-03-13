{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text

data StepType
    = StartType
    | RegularStep
    | DecisionStep
    | EndType

data Step = Step {
    originCoordinates :: Point V2 Double,
    name :: Double,
    stepType :: StepType }

stepName :: Step -> Double
stepName Step { originCoordinates = x, name = y, stepType = z } = y

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

steps :: [Step]
steps =
    Step { originCoordinates = p2 (0, 0), name = uniqueName 0 0, stepType = StartType }
    : Step { originCoordinates = p2 (0, -1), name = uniqueName 0 (-1), stepType = DecisionStep }
    : [Step { originCoordinates = p2 (x, y), name = uniqueName x y, stepType = RegularStep } | x <- [0], y <- [-2, -3, -4]]
    ++ [Step { originCoordinates = p2 (0, -5), name = uniqueName 0 (-5), stepType = EndType }]

connections :: [Step] -> [QDiagram B V2 Double Any -> QDiagram B V2 Double Any]
connections (x1: x2: xn) = connectOutside' (with & arrowHead .~ noHead) (stepName x1) (stepName x2): connections (x2: xn)
connections (x1: []) = []
connections [] = []

correctShape :: StepType -> Double -> Diagram B
correctShape StartType = startShape
correctShape EndType = endShape
correctShape DecisionStep = decisionShape
correctShape _ = stepShape

main = mainWith $
    position [(x, correctShape z y) | Step { originCoordinates = x, name = y, stepType = z } <- steps]
    # applyAll (connections steps)
    # lw veryThin
