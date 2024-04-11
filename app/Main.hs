module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import GHC.Data.Graph.Directed
import GHC.Utils.Outputable
import GHC.Utils.Ppr

import System.IO

-- constructing the graph ->

node1 :: Node Int String
node1 = DigraphNode { node_payload = "title", node_key = 1, node_dependencies = [] }

node2 :: Node Int String
node2 = DigraphNode { node_payload = "Do you have money?", node_key = 2, node_dependencies = [1] }

node3 :: Node Int String
node3 = DigraphNode { node_payload = "Are puppies on sale today?", node_key = 3, node_dependencies = [2] }

node4 :: Node Int String
node4 = DigraphNode { node_payload = "Find a puppy", node_key = 4, node_dependencies = [3] }

node5 :: Node Int String
node5 = DigraphNode { node_payload = "Do you like the puppy?", node_key = 5, node_dependencies = [4] }

node6 :: Node Int String
node6 = DigraphNode { node_payload = "Buy this cute puppy!", node_key = 6, node_dependencies = [5] }

node7 :: Node Int String
node7 = DigraphNode { node_payload = "end", node_key = 7, node_dependencies = [6] }

graph :: Graph (Node Int String)
graph = graphFromEdgedVerticesUniq [node1, node2, node3, node4, node5, node6, node7]

-- <- constructing the graph

-- graph manipulation ->

payload :: Node Int String -> String
payload DigraphNode { node_payload = x, node_key = _, node_dependencies = _ } = x

isTitleIcon :: Node Int String -> Bool
isTitleIcon x = "title" == payload x

-- TODO 1: add unit tests
-- TODO 2: add documentation
titleIcon ::
  Graph (Node Int String) ->
  Maybe (Node Int String)
titleIcon x = do
  let icons = verticesG x
  let titleIcons = filter isTitleIcon icons
  
  case titleIcons of
    [y] -> Just y   -- expected result - only one title icon found
    _:_ -> Nothing  -- there can only be one title icon
    []  -> Nothing  -- no title icons

visualGraph :: Graph (Node Int String) -> Diagram B
visualGraph x = do
  case titleIcon x of
    Nothing -> startShape -- but really this should be no shape
    Just _ -> startShape

-- <- graph manipulation

-- visual constants ->

lengthUnit :: Double
lengthUnit = 1.0

cellWidth :: Double
cellWidth = lengthUnit

cellHeight :: Double
cellHeight = lengthUnit

iconWidth :: Double
iconWidth = 2.0 * cellWidth

iconHeight :: Double
iconHeight = cellHeight

-- <- visual constants

troubleshootingMode :: Bool
troubleshootingMode = True

startShape :: Diagram B
startShape = do
  let shape = roundedRect iconWidth iconHeight 0.5

  if troubleshootingMode
    then showOrigin shape
    else shape

main :: IO ()
main = do
  printSDocLn defaultSDocContext LeftMode stderr $ ppr graph
  mainWith $ position [(p2 (0.0, 0.0), visualGraph graph)]