{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data Step = Step {
    originCoordinates :: Point V2 Double,
    name :: Double }

stepName :: Step -> Double
stepName Step { originCoordinates = x, name = y } = y

stepShape :: Double -> Diagram B
stepShape x = rect 0.75 0.5 # showOrigin # named x

uniqueName :: Double -> Double -> Double
uniqueName x y = x * 10 + y

steps :: [Step]
steps = [ Step { originCoordinates = p2 (x, y), name = uniqueName x y } | x <- [0], y <- [0, -1, -2, -3]]

connections :: [Step] -> [QDiagram B V2 Double Any -> QDiagram B V2 Double Any]
connections (x1: x2: xn) = connectOutside (stepName x1) (stepName x2): connections (x2: xn)
connections (x1: []) = []
connections [] = []

main = mainWith $
    position [(x, (stepShape y)) | Step { originCoordinates = x, name = y } <- steps]
    # applyAll (connections steps)
    # lw veryThin
