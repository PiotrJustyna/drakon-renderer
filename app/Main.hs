module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr

import System.IO

-- constructing the graph ->

node1 :: Node Int String
node1 = DigraphNode { node_payload = "title", node_key = 1, node_dependencies = [2, 3] }

node2 :: Node Int String
node2 = DigraphNode { node_payload = "Do you have money?", node_key = 2, node_dependencies = [3] }

node3 :: Node Int String
node3 = DigraphNode { node_payload = "Are puppies on sale today?", node_key = 3, node_dependencies = [4] }

node4 :: Node Int String
node4 = DigraphNode { node_payload = "Find a puppy", node_key = 4, node_dependencies = [5] }

node5 :: Node Int String
node5 = DigraphNode { node_payload = "Do you like the puppy?", node_key = 5, node_dependencies = [6] }

node6 :: Node Int String
node6 = DigraphNode { node_payload = "Buy this cute puppy!", node_key = 6, node_dependencies = [7] }

node7 :: Node Int String
node7 = DigraphNode { node_payload = "end", node_key = 7, node_dependencies = [] }

graph :: Graph (Node Int String)
graph = graphFromEdgedVerticesUniq [node1, node2, node3, node4, node5, node6, node7]

icons :: [Node Int String]
icons = verticesG graph

-- <- constructing the graph

-- graph manipulation ->

payload :: Node Int String -> String
payload DigraphNode { node_payload = x, node_key = _, node_dependencies = _ } = x

dependencies :: Node Int String -> [Int]
dependencies DigraphNode { node_payload = _, node_key = _, node_dependencies = x } = x

isTitleIcon :: Node Int String -> Bool
isTitleIcon x = "title" == payload x

-- TODO 1: add unit tests
-- TODO 2: add documentation
titleIcon :: Maybe (Node Int String)
titleIcon = do
  let titleIcons = filter isTitleIcon icons
  
  case titleIcons of
    [y] -> Just y   -- expected result - only one title icon found
    _:_ -> Nothing  -- there can only be one title icon
    []  -> Nothing  -- no title icons

visualGraph :: [(Point V2 Double, Diagram B)]
visualGraph = do
  case titleIcon of
    Nothing -> []
    Just x -> (p2 (0.0, 0.0), startShape $ payload x) : (xyz (take 3 icons) 0.0)

xyz :: [Node Int String] -> Double -> [(Point V2 Double, Diagram B)]
xyz [] _ = []
xyz [x] width = [(p2 (width, -1.0), startShape "dupa")]
xyz (x:xs) width = (p2 (width, -1.0), startShape "dupa") : (xyz xs (width + iconWidth))

--xyz :: Node Int String -> [(Point V2 Double, Diagram B)]
--xyz x = do
--  let xDependencies = dependencies x
  -- let xPayload      = payload x

--  foldl (\acc _ -> (p2 (0.0, -1.0), startShape "dupa") : acc) [] xDependencies

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

startShape :: String -> Diagram B
startShape x = do
  let shape = text x # fontSize (local 0.1) # light # font "courier" <> roundedRect iconWidth iconHeight 0.5

  if troubleshootingMode
    then showOrigin shape
    else shape

main :: IO ()
main = do
  GHC.Utils.Outputable.printSDocLn GHC.Utils.Outputable.defaultSDocContext GHC.Utils.Ppr.LeftMode stderr $ GHC.Utils.Outputable.ppr graph
  mainWith $ position visualGraph