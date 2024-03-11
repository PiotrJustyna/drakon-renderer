{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text

data StepType
    = StartType
    | RegularStep
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
startShape x = text "hello" <> roundedRect 0.95 0.4 0.5 # showOrigin # named x

endShape :: Double -> Diagram B
endShape = startShape

uniqueName :: Double -> Double -> Double
uniqueName x y = x * 10 + (abs y)

steps :: [Step]
steps =
    Step { originCoordinates = p2 (0, 0), name = uniqueName 0 0, stepType = StartType }
    : [Step { originCoordinates = p2 (x, y), name = uniqueName x y, stepType = RegularStep } | x <- [0], y <- [-1, -2, -3]]
    ++ [Step { originCoordinates = p2 (0, -4), name = uniqueName 0 4, stepType = EndType }]

connections :: [Step] -> [QDiagram B V2 Double Any -> QDiagram B V2 Double Any]
connections (x1: x2: xn) = connectOutside (stepName x1) (stepName x2): connections (x2: xn)
connections (x1: []) = []
connections [] = []

correctShape :: StepType -> Double -> Diagram B
correctShape StartType = startShape
correctShape EndType = endShape
correctShape _ = stepShape

main = mainWith $
    position [(x, correctShape z y) | Step { originCoordinates = x, name = y, stepType = z } <- steps]
    # applyAll (connections steps)
    # lw veryThin
