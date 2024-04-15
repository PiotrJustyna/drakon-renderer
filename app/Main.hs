module Main where

import qualified Data.Map

import qualified Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Prelude

import GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr

import System.IO

-- constructing the graph ->

node1 :: Node Int String
node1 = DigraphNode { node_payload = "title", node_key = titleIconKey, node_dependencies = [2, 3] }

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

icons :: Data.Map.Map Int (Node Int String)
icons = Data.Map.fromList . map (\icon -> (key icon, icon)) $ verticesG graph

iconsWithKeys :: [Int] -> [Node Int String]
iconsWithKeys ks = Data.Map.foldlWithKey (\acc k a -> if k `elem` ks then a:acc else acc) [] icons

-- <- constructing the graph

-- graph manipulation ->

payload :: Node Int String -> String
payload DigraphNode { node_payload = x, node_key = _, node_dependencies = _ } = x

key :: Node Int String -> Int
key DigraphNode { node_payload = _, node_key = x, node_dependencies = _ } = x

dependencies :: Node Int String -> [Int]
dependencies DigraphNode { node_payload = _, node_key = _, node_dependencies = x } = x

visualGraph :: [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double, Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)]
visualGraph = do
  let titleIcon                 = icons Data.Map.! titleIconKey
  let titleIconPayload          = payload titleIcon
  let titleIconDependenciesKeys = dependencies titleIcon
  let titleIconDependencies     = iconsWithKeys titleIconDependenciesKeys
  (Diagrams.Prelude.p2 (0.0, 0.0), startShape titleIconPayload) : visualSubgraph titleIconDependencies 0.0

visualSubgraph :: [Node Int String] -> Double -> [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double, Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)]
visualSubgraph [] _          = []
visualSubgraph [x] width     = [(Diagrams.Prelude.p2 (width, -1.0), startShape $ payload x)]
visualSubgraph (x:xs) width  = (Diagrams.Prelude.p2 (width, -1.0), startShape $ payload x) : visualSubgraph xs (width + cellWidth)

-- <- graph manipulation

-- visual constants ->

lengthUnit :: Double
lengthUnit = 1.0

cellWidth :: Double
cellWidth = 2.0 * lengthUnit

cellHeight :: Double
cellHeight = lengthUnit

iconWidth :: Double
iconWidth = 0.8 * cellWidth

iconHeight :: Double
iconHeight = 0.4 * cellHeight

-- <- visual constants

titleIconKey :: Int
titleIconKey = 1

troubleshootingMode :: Bool
troubleshootingMode = True

startShape :: String -> Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
startShape x = do
  let shape = Diagrams.Prelude.text x Diagrams.Prelude.# Diagrams.Prelude.fontSize (Diagrams.Prelude.local 0.075) Diagrams.Prelude.# Diagrams.Prelude.light Diagrams.Prelude.# Diagrams.Prelude.font "courier" <> Diagrams.Prelude.roundedRect iconWidth iconHeight 0.5

  if troubleshootingMode
    then Diagrams.Prelude.showOrigin shape
    else shape

main :: IO ()
main = do
  GHC.Utils.Outputable.printSDocLn GHC.Utils.Outputable.defaultSDocContext GHC.Utils.Ppr.LeftMode stderr $ GHC.Utils.Outputable.ppr graph
  Diagrams.Backend.SVG.CmdLine.mainWith $ Diagrams.Prelude.position visualGraph