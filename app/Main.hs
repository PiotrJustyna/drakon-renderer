module Main where

import qualified Data.Colour.SRGB

import qualified Data.Map

import qualified Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Prelude

import qualified GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr

import qualified System.IO

data IconType = Title | End | Action | Question

instance Show IconType where
    show Title = "Title"
    show End = "End"
    show Action = "Action"
    show Question = "Question"

data Icon = Icon { iconText :: String, iconType :: IconType }

instance GHC.Utils.Outputable.Outputable Icon where
    ppr Icon { iconText = x, iconType = y } = GHC.Utils.Outputable.text $ show y ++ ": " ++ x

-- constructing the graph ->

node1 ::
  GHC.Data.Graph.Directed.Node Int Icon
node1 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "start",
      iconType = Title
    },
    GHC.Data.Graph.Directed.node_key = titleIconKey,
    GHC.Data.Graph.Directed.node_dependencies = [2]
  }

node2 ::
  GHC.Data.Graph.Directed.Node Int Icon
node2 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "question 1",
      iconType = Question
    },
    GHC.Data.Graph.Directed.node_key = 2,
    GHC.Data.Graph.Directed.node_dependencies = [3, 9]
  }

node3 ::
  GHC.Data.Graph.Directed.Node Int Icon
node3 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "action 1",
      iconType = Action
    },
    GHC.Data.Graph.Directed.node_key = 3,
    GHC.Data.Graph.Directed.node_dependencies = [4]
  }

node4 ::
  GHC.Data.Graph.Directed.Node Int Icon
node4 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "question 2",
      iconType = Question
    },
    GHC.Data.Graph.Directed.node_key = 4,
    GHC.Data.Graph.Directed.node_dependencies = [5, 7]
  }

node5 ::
  GHC.Data.Graph.Directed.Node Int Icon
node5 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "action 2",
      iconType = Action
    },
    GHC.Data.Graph.Directed.node_key = 5,
    GHC.Data.Graph.Directed.node_dependencies = [6]
  }

node6 ::
  GHC.Data.Graph.Directed.Node Int Icon
node6 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "end 1",
      iconType = End
    },
    GHC.Data.Graph.Directed.node_key = 6,
    GHC.Data.Graph.Directed.node_dependencies = []
  }

node7 ::
  GHC.Data.Graph.Directed.Node Int Icon
node7 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "action 3",
      iconType = Action
    },
    GHC.Data.Graph.Directed.node_key = 7,
    GHC.Data.Graph.Directed.node_dependencies = [8]
  }

node8 ::
  GHC.Data.Graph.Directed.Node Int Icon
node8 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "end 2",
      iconType = End
    },
    GHC.Data.Graph.Directed.node_key = 8,
    GHC.Data.Graph.Directed.node_dependencies = []
  }

node9 ::
  GHC.Data.Graph.Directed.Node Int Icon
node9 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "question 3",
      iconType = Question
    },
    GHC.Data.Graph.Directed.node_key = 9,
    GHC.Data.Graph.Directed.node_dependencies = [10, 12]
  }

node10 ::
  GHC.Data.Graph.Directed.Node Int Icon
node10 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "action 4",
      iconType = Action
    },
    GHC.Data.Graph.Directed.node_key = 10,
    GHC.Data.Graph.Directed.node_dependencies = [11]
  }

node11 ::
  GHC.Data.Graph.Directed.Node Int Icon
node11 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "end 3",
      iconType = End
    },
    GHC.Data.Graph.Directed.node_key = 11,
    GHC.Data.Graph.Directed.node_dependencies = []
  }

node12 ::
  GHC.Data.Graph.Directed.Node Int Icon
node12 = GHC.Data.Graph.Directed.DigraphNode
  {
    GHC.Data.Graph.Directed.node_payload = Icon
    {
      iconText = "end 4",
      iconType = End
    },
    GHC.Data.Graph.Directed.node_key = 12,
    GHC.Data.Graph.Directed.node_dependencies = []
  }

graph ::
  GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node Int Icon)
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
  Data.Map.Map Int (GHC.Data.Graph.Directed.Node Int Icon)
icons = Data.Map.fromList . map (\icon -> (key icon, icon)) $ GHC.Data.Graph.Directed.verticesG graph

iconsWithKeys ::
  [Int] ->
  [GHC.Data.Graph.Directed.Node Int Icon]
iconsWithKeys ks = Data.Map.foldrWithKey (\k a acc -> if k `elem` ks then a:acc else acc) [] icons

-- <- constructing the graph

-- graph manipulation ->

payload :: GHC.Data.Graph.Directed.Node Int Icon -> Icon
payload GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = x, GHC.Data.Graph.Directed.node_key = _, GHC.Data.Graph.Directed.node_dependencies = _ } = x

key :: GHC.Data.Graph.Directed.Node Int Icon -> Int
key GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = _, GHC.Data.Graph.Directed.node_key = x, GHC.Data.Graph.Directed.node_dependencies = _ } = x

dependencies :: GHC.Data.Graph.Directed.Node Int Icon -> [Int]
dependencies GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = _, GHC.Data.Graph.Directed.node_key = _, GHC.Data.Graph.Directed.node_dependencies = x } = x

visualGraph ::
  [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)]
visualGraph = do
  let renderingOrder = titleIconKey
  let titleIcon = icons Data.Map.! titleIconKey
  let startingWidth = 0.0
  let startingDepth = 0.0
  let firstChildIconDepth = startingDepth + (-1.0) * cellHeight
  let Icon { iconText = titleIconText, iconType = _ } = payload titleIcon
  let titleIconDependenciesKeys = dependencies titleIcon
  let titleIconDependencies = iconsWithKeys titleIconDependenciesKeys
  let (_, _, childSubgraphVisualData) = visualSubgraph titleIconDependencies (renderingOrder + 1) startingWidth firstChildIconDepth startingWidth startingDepth

  (Diagrams.Prelude.p2 (0.0, 0.0), titleShape titleIconText renderingOrder startingWidth) : childSubgraphVisualData

visualSubgraph ::
  [GHC.Data.Graph.Directed.Node Int Icon] ->
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  (Int, Double, [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)])
visualSubgraph [] renderingOrder width _ _ _ =
  (renderingOrder,
    width,
    [])

visualSubgraph [x] renderingOrder width depth previousIconOriginCoordinateX previousIconOriginCoordinateY = do
  let (childSubgraphMaxUsedRenderingOrder,
        childSubgraphMaxUsedWidth,
        childSubgraphVisualData) =
          visualSubgraph (iconsWithKeys (dependencies x)) (renderingOrder + 1) width (depth - cellHeight) width depth

  (childSubgraphMaxUsedRenderingOrder,
    childSubgraphMaxUsedWidth,
    visualSubgraphNode width depth (previousIconOriginCoordinateX - width) (previousIconOriginCoordinateY - depth) (payload x) renderingOrder width : childSubgraphVisualData)

visualSubgraph (x:xs) renderingOrder width depth previousIconOriginCoordinateX previousIconOriginCoordinateY = do
  let (leftChildSubgraphMaxUsedRenderingOrder,
        leftChildSubgraphMaxUsedWidth,
        leftChildSubgraphVisualData) =
          visualSubgraph (iconsWithKeys (dependencies x)) (renderingOrder + 1) width (depth - cellHeight) width depth
  let newRightChildSubgraphWidth = leftChildSubgraphMaxUsedWidth + cellWidth
  let (rightChildSubgraphMaxUsedRenderingOrder,
        rightChildSubgraphMaxUsedWidth,
        rightChildSubgraphVisualData) =
          visualSubgraph xs leftChildSubgraphMaxUsedRenderingOrder newRightChildSubgraphWidth depth width (depth + cellHeight)

  (rightChildSubgraphMaxUsedRenderingOrder,
    rightChildSubgraphMaxUsedWidth,
    visualSubgraphNode width depth (previousIconOriginCoordinateX - width) (previousIconOriginCoordinateY - depth) (payload x) renderingOrder width: leftChildSubgraphVisualData ++ rightChildSubgraphVisualData)

visualSubgraphNode ::
  Double ->
  Double ->
  Double ->
  Double ->
  Icon ->
  Int ->
  Double ->
  (Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)
visualSubgraphNode width depth previousIconOriginCoordinateX previousIconOriginCoordinateY icon renderingOrder maxWidth = do
  let Icon { iconText = x, iconType = y } = icon
  (Diagrams.Prelude.p2 (width, depth), correctShape y previousIconOriginCoordinateX previousIconOriginCoordinateY x renderingOrder maxWidth)

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

fontSize ::
  Double
fontSize = 0.1

backgroundColour ::
  Diagrams.Prelude.Colour Double
backgroundColour = Data.Colour.SRGB.sRGB (230.0/255.0) (232.0/255.0) (216.0/255.0)

fontColour ::
  Diagrams.Prelude.Colour Double
fontColour = Data.Colour.SRGB.sRGB (34.0/255.0) (69.0/255.0) (57.0/255.0)

lineColour ::
  Diagrams.Prelude.Colour Double
lineColour = Data.Colour.SRGB.sRGB (34.0/255.0) (69.0/255.0) (57.0/255.0)

titleIconColour ::
  Diagrams.Prelude.Colour Double
titleIconColour = Data.Colour.SRGB.sRGB (69.0/255.0) (173.0/255.0) (127.0/255.0)

actionIconColour ::
  Diagrams.Prelude.Colour Double
actionIconColour = titleIconColour

questionIconColour ::
  Diagrams.Prelude.Colour Double
questionIconColour = titleIconColour

endIconColour ::
  Diagrams.Prelude.Colour Double
endIconColour = titleIconColour

-- <- visual constants

titleIconKey ::
  Int
titleIconKey = 1

troubleshootingMode ::
  Bool
troubleshootingMode = True

connectionToParentIcon ::
  Double ->
  Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
connectionToParentIcon x y =
  Diagrams.Prelude.fromOffsets
    [Diagrams.Prelude.V2 0 (y - (if abs x > 0 then iconHeight * 0.5 else iconHeight))]
    Diagrams.Prelude.#
    Diagrams.Prelude.translate
    (Diagrams.Prelude.r2 (0, iconHeight * 0.5))
  <>
  Diagrams.Prelude.fromOffsets
    [Diagrams.Prelude.V2 (if abs x > 0 then x + (iconWidth * 0.5) else 0) 0]
    Diagrams.Prelude.#
    Diagrams.Prelude.translate
    (Diagrams.Prelude.r2 (0, y))

correctShape ::
  IconType ->
  Double ->
  Double ->
  String ->
  Int ->
  Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
correctShape Title _ _ titleIconText renderingOrder maxWidth =
  titleShape titleIconText renderingOrder maxWidth
correctShape End parentIconVectorX parentIconVectorY endIconText renderingOrder maxWidth =
  endShape parentIconVectorX parentIconVectorY endIconText renderingOrder maxWidth
correctShape Question parentIconVectorX parentIconVectorY questionIconText renderingOrder maxWidth =
  questionShape parentIconVectorX parentIconVectorY questionIconText renderingOrder maxWidth
correctShape Action parentIconVectorX parentIconVectorY actionIconText renderingOrder maxWidth =
  actionShape parentIconVectorX parentIconVectorY actionIconText renderingOrder maxWidth

text ::
  String ->
  Double ->
  Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
text
  content
  translateX
  translateY =
    Diagrams.Prelude.text content
    Diagrams.Prelude.#
    Diagrams.Prelude.fontSize (Diagrams.Prelude.local fontSize)
    Diagrams.Prelude.#
    Diagrams.Prelude.light
    Diagrams.Prelude.#
    Diagrams.Prelude.font "helvetica"
    Diagrams.Prelude.#
    Diagrams.Prelude.fc fontColour
    Diagrams.Prelude.#
    Diagrams.Prelude.translate (Diagrams.Prelude.r2 (translateX,  translateY))

titleShape ::
  String ->
  Int ->
  Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
titleShape
  titleIconText
  renderingOrder
  maxWidth = do
  let baseShape =
        Diagrams.Prelude.roundedRect iconWidth iconHeight 0.5
        Diagrams.Prelude.#
        Diagrams.Prelude.fc titleIconColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lc lineColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

  let shape = if troubleshootingMode
        then Diagrams.Prelude.showOrigin baseShape
        else baseShape

  if troubleshootingMode then
    (text titleIconText 0.0 0.0
      Diagrams.Prelude.===
      (text ("rendering order: " ++ show renderingOrder) 0.0 0.0 <> Diagrams.Prelude.strutY (fontSize * 2.0))
      Diagrams.Prelude.===
      text ("max width: " ++ show maxWidth) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> shape
  else
    text titleIconText 0.0 0.0
    <> shape

actionShape ::
  Double ->
  Double ->
  String ->
  Int ->
  Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
actionShape
  parentIconVectorX
  parentIconVectorY
  actionIconText
  renderingOrder
  maxWidth = do
  let baseShape =
        Diagrams.Prelude.rect iconWidth iconHeight
        Diagrams.Prelude.#
        Diagrams.Prelude.fc actionIconColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lc lineColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

  let shape = if troubleshootingMode
        then Diagrams.Prelude.showOrigin baseShape
        else baseShape

  if troubleshootingMode then
    (text actionIconText 0.0 0.0
      Diagrams.Prelude.===
      (text ("rendering order: " ++ show renderingOrder) 0.0 0.0 <> Diagrams.Prelude.strutY (fontSize * 2.0))
      Diagrams.Prelude.===
      text ("max width: " ++ show maxWidth) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> shape
      <> connectionToParentIcon parentIconVectorX parentIconVectorY
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.ultraThin
  else
    text actionIconText 0.0 0.0
    <> shape
    <> connectionToParentIcon parentIconVectorX parentIconVectorY
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

questionShape ::
  Double ->
  Double ->
  String ->
  Int ->
  Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
questionShape
  parentIconVectorX
  parentIconVectorY
  questionIconText
  renderingOrder
  maxWidth = do
  let baseShape =
        Diagrams.Prelude.fromOffsets
        [Diagrams.Prelude.V2 (-0.1) (iconHeight * 0.5),
        Diagrams.Prelude.V2 0.1 (iconHeight * 0.5),
        Diagrams.Prelude.V2 (iconWidth - 0.1 - 0.1) 0.0,
        Diagrams.Prelude.V2 0.1 (iconHeight * (-0.5)),
        Diagrams.Prelude.V2 (-0.1) (iconHeight * (-0.5)),
        Diagrams.Prelude.V2 ((iconWidth - 0.1 - 0.1) * (-1.0)) 0.0]
        Diagrams.Prelude.#
        Diagrams.Prelude.closeLine
        Diagrams.Prelude.#
        Diagrams.Prelude.strokeLoop
        Diagrams.Prelude.#
        Diagrams.Prelude.fc questionIconColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lc lineColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lw Diagrams.Prelude.ultraThin
        Diagrams.Prelude.#
        Diagrams.Prelude.translate (Diagrams.Prelude.r2 ((iconWidth - 0.1 - 0.1) * (-0.5), -0.2))

  let shape = if troubleshootingMode
        then Diagrams.Prelude.showOrigin baseShape
        else baseShape

  if troubleshootingMode then
    (text questionIconText 0.0 0.0
      Diagrams.Prelude.===
      (text ("rendering order: " ++ show renderingOrder) 0.0 0.0 <> Diagrams.Prelude.strutY (fontSize * 2.0))
      Diagrams.Prelude.===
      text ("max width: " ++ show maxWidth) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> text "yes" (iconWidth * (-0.1)) (iconHeight * (-0.7))
      <> text "no" (iconWidth * 0.55) (iconHeight * 0.15)
      <> shape
      <> connectionToParentIcon parentIconVectorX parentIconVectorY
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.ultraThin
  else
    text questionIconText 0.0 0.0
    <> text "yes" (iconWidth * (-0.1)) (iconHeight * (-0.7))
    <> text "no" (iconWidth * 0.55) (iconHeight * 0.15)
    <> shape
    <> connectionToParentIcon parentIconVectorX parentIconVectorY
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

endShape ::
  Double ->
  Double ->
  String ->
  Int ->
  Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
endShape
  parentIconVectorX
  parentIconVectorY
  endIconText
  renderingOrder
  maxWidth = do
  let baseShape =
        Diagrams.Prelude.roundedRect iconWidth iconHeight 0.5
        Diagrams.Prelude.#
        Diagrams.Prelude.fc endIconColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lc lineColour
        Diagrams.Prelude.#
        Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

  let shape = if troubleshootingMode
        then Diagrams.Prelude.showOrigin baseShape
        else baseShape

  if troubleshootingMode then
    (text endIconText 0.0 0.0
      Diagrams.Prelude.===
      (text ("rendering order: " ++ show renderingOrder) 0.0 0.0 <> Diagrams.Prelude.strutY (fontSize * 2.0))
      Diagrams.Prelude.===
      text ("max width: " ++ show maxWidth) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> shape
      <> connectionToParentIcon parentIconVectorX parentIconVectorY
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.ultraThin
  else
    text endIconText 0.0 0.0
    <> shape
    <> connectionToParentIcon parentIconVectorX parentIconVectorY
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

main ::
  IO ()
main = do
  GHC.Utils.Outputable.printSDocLn GHC.Utils.Outputable.defaultSDocContext GHC.Utils.Ppr.LeftMode System.IO.stdout $ GHC.Utils.Outputable.ppr graph
  Diagrams.Backend.SVG.CmdLine.mainWith $
    Diagrams.Prelude.position visualGraph
    Diagrams.Prelude.#
    Diagrams.Prelude.bg backgroundColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.none