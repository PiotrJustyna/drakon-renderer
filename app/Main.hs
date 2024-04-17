module Main where

import qualified Data.Map

import qualified Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Prelude

import qualified GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr

import qualified System.IO

-- constructing the graph ->

node1 ::
  GHC.Data.Graph.Directed.Node Int String
node1 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "start", GHC.Data.Graph.Directed.node_key = titleIconKey, GHC.Data.Graph.Directed.node_dependencies = [2] }

node2 ::
  GHC.Data.Graph.Directed.Node Int String
node2 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "decision 1", GHC.Data.Graph.Directed.node_key = 2, GHC.Data.Graph.Directed.node_dependencies = [3, 9] }

node3 ::
  GHC.Data.Graph.Directed.Node Int String
node3 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "action 1", GHC.Data.Graph.Directed.node_key = 3, GHC.Data.Graph.Directed.node_dependencies = [4] }

node4 ::
  GHC.Data.Graph.Directed.Node Int String
node4 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "decision 2", GHC.Data.Graph.Directed.node_key = 4, GHC.Data.Graph.Directed.node_dependencies = [5, 7] }

node5 ::
  GHC.Data.Graph.Directed.Node Int String
node5 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "action 2", GHC.Data.Graph.Directed.node_key = 5, GHC.Data.Graph.Directed.node_dependencies = [6] }

node6 ::
  GHC.Data.Graph.Directed.Node Int String
node6 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "end 1", GHC.Data.Graph.Directed.node_key = 6, GHC.Data.Graph.Directed.node_dependencies = [] }

node7 ::
  GHC.Data.Graph.Directed.Node Int String
node7 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "action 3", GHC.Data.Graph.Directed.node_key = 7, GHC.Data.Graph.Directed.node_dependencies = [8] }

node8 ::
  GHC.Data.Graph.Directed.Node Int String
node8 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "end 2", GHC.Data.Graph.Directed.node_key = 8, GHC.Data.Graph.Directed.node_dependencies = [] }

node9 ::
  GHC.Data.Graph.Directed.Node Int String
node9 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "decision 3", GHC.Data.Graph.Directed.node_key = 9, GHC.Data.Graph.Directed.node_dependencies = [10, 12] }

node10 ::
  GHC.Data.Graph.Directed.Node Int String
node10 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "action 4", GHC.Data.Graph.Directed.node_key = 10, GHC.Data.Graph.Directed.node_dependencies = [11] }

node11 ::
  GHC.Data.Graph.Directed.Node Int String
node11 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "end 3", GHC.Data.Graph.Directed.node_key = 11, GHC.Data.Graph.Directed.node_dependencies = [] }

node12 ::
  GHC.Data.Graph.Directed.Node Int String
node12 = GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = "end 4", GHC.Data.Graph.Directed.node_key = 12, GHC.Data.Graph.Directed.node_dependencies = [] }

graph ::
  GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node Int String)
graph = GHC.Data.Graph.Directed.graphFromEdgedVerticesUniq
  [node1,
  node2,
  node3,
  node4,
  node5,
  node6,
  node7,
  node8,
  node9,
  node10,
  node11,
  node12]

icons ::
  Data.Map.Map Int (GHC.Data.Graph.Directed.Node Int String)
icons = Data.Map.fromList . map (\icon -> (key icon, icon)) $ GHC.Data.Graph.Directed.verticesG graph

iconsWithKeys ::
  [Int] ->
  [GHC.Data.Graph.Directed.Node Int String]
iconsWithKeys ks = Data.Map.foldrWithKey (\k a acc -> if k `elem` ks then a:acc else acc) [] icons

-- <- constructing the graph

-- graph manipulation ->

conditionalSuffix :: String -> String -> Bool -> String
conditionalSuffix input suffix condition = if condition then input ++ suffix else input

conditionalRenderingSuffix :: String -> Int -> Double -> Bool -> String
conditionalRenderingSuffix input renderingOrder maxWidth =
  conditionalSuffix input (" | rendering order: " ++ show renderingOrder ++ " | max width: " ++ show maxWidth)

payload :: GHC.Data.Graph.Directed.Node Int String -> String
payload GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = x, GHC.Data.Graph.Directed.node_key = _, GHC.Data.Graph.Directed.node_dependencies = _ } = x

key :: GHC.Data.Graph.Directed.Node Int String -> Int
key GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = _, GHC.Data.Graph.Directed.node_key = x, GHC.Data.Graph.Directed.node_dependencies = _ } = x

dependencies :: GHC.Data.Graph.Directed.Node Int String -> [Int]
dependencies GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = _, GHC.Data.Graph.Directed.node_key = _, GHC.Data.Graph.Directed.node_dependencies = x } = x

visualGraph ::
  [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)]
visualGraph = do
  let renderingOrder            = titleIconKey
  let titleIcon                 = icons Data.Map.! titleIconKey
  let startingWidth             = 0.0
  let startingDepth             = (-1.0) * cellHeight
  let titleIconPayload          = conditionalRenderingSuffix (payload titleIcon) renderingOrder startingWidth troubleshootingMode
  let titleIconDependenciesKeys = dependencies titleIcon
  let titleIconDependencies     = iconsWithKeys titleIconDependenciesKeys
  let (_, _, childSubgraphVisualData) = visualSubgraph titleIconDependencies (renderingOrder + 1) startingWidth startingDepth

  (Diagrams.Prelude.p2 (0.0, 0.0), startShape titleIconPayload) : childSubgraphVisualData

visualSubgraph ::
  [GHC.Data.Graph.Directed.Node Int String] ->
  Int ->
  Double ->
  Double ->
  (Int, Double, [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)])
visualSubgraph [] renderingOrder width _ =
  (renderingOrder,
    width,
    [])

visualSubgraph [x] renderingOrder width depth = do
  let (childSubgraphMaxUsedRenderingOrder,
        childSubgraphMaxUsedWidth,
        childSubgraphVisualData) =
          visualSubgraph (iconsWithKeys (dependencies x)) (renderingOrder + 1) width (depth - cellHeight)

  (childSubgraphMaxUsedRenderingOrder,
    childSubgraphMaxUsedWidth,
    visualSubgraphNode width depth (payload x) renderingOrder width : childSubgraphVisualData)

visualSubgraph (x:xs) renderingOrder width depth = do
  let (leftChildSubgraphMaxUsedRenderingOrder,
        leftChildSubgraphMaxUsedWidth,
        leftChildSubgraphVisualData) =
          visualSubgraph (iconsWithKeys (dependencies x)) (renderingOrder + 1) width (depth - cellHeight)
  let newRightChildSubgraphWidth = leftChildSubgraphMaxUsedWidth + cellWidth
  let (rightChildSubgraphMaxUsedRenderingOrder,
        rightChildSubgraphMaxUsedWidth,
        rightChildSubgraphVisualData) =
          visualSubgraph xs leftChildSubgraphMaxUsedRenderingOrder newRightChildSubgraphWidth depth

  (rightChildSubgraphMaxUsedRenderingOrder,
    rightChildSubgraphMaxUsedWidth,
    visualSubgraphNode width depth (payload x) renderingOrder width: leftChildSubgraphVisualData ++ rightChildSubgraphVisualData)

visualSubgraphNode ::
  Double ->
  Double ->
  String ->
  Int ->
  Double ->
  (Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)
visualSubgraphNode width depth text renderingOrder maxWidth =
  (Diagrams.Prelude.p2 (width, depth), actionShape $ conditionalRenderingSuffix text renderingOrder maxWidth troubleshootingMode)

-- <- graph manipulation

-- visual constants ->

lengthUnit ::
  Double
lengthUnit = 1.0

cellWidth ::
  Double
cellWidth = 2.0 * lengthUnit

cellHeight ::
  Double
cellHeight = lengthUnit

iconWidth ::
  Double
iconWidth = 0.8 * cellWidth

iconHeight ::
  Double
iconHeight = 0.4 * cellHeight

-- <- visual constants

titleIconKey ::
  Int
titleIconKey = 1

troubleshootingMode ::
  Bool
troubleshootingMode = True

startShape ::
  String ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
startShape x = do
  let shape =
        Diagrams.Prelude.text x
        Diagrams.Prelude.#
        Diagrams.Prelude.fontSize (Diagrams.Prelude.local 0.05)
        Diagrams.Prelude.#
        Diagrams.Prelude.light
        Diagrams.Prelude.#
        Diagrams.Prelude.font "courier"
        <>
        Diagrams.Prelude.roundedRect iconWidth iconHeight 0.5

  if troubleshootingMode
    then Diagrams.Prelude.showOrigin shape
    else shape

actionShape ::
  String ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
actionShape x = do
  let shape =
        Diagrams.Prelude.text x
        Diagrams.Prelude.#
        Diagrams.Prelude.fontSize (Diagrams.Prelude.local 0.05)
        Diagrams.Prelude.#
        Diagrams.Prelude.light
        Diagrams.Prelude.#
        Diagrams.Prelude.font "courier"
        <>
        Diagrams.Prelude.rect iconWidth iconHeight

  if troubleshootingMode
    then Diagrams.Prelude.showOrigin shape
    else shape

main ::
  IO ()
main = do
  GHC.Utils.Outputable.printSDocLn GHC.Utils.Outputable.defaultSDocContext GHC.Utils.Ppr.LeftMode System.IO.stdout $ GHC.Utils.Outputable.ppr graph
  Diagrams.Backend.SVG.CmdLine.mainWith $ Diagrams.Prelude.position visualGraph Diagrams.Prelude.# Diagrams.Prelude.lw Diagrams.Prelude.veryThin