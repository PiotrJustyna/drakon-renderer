module Renderer where

import qualified Data.Colour.SRGB
import qualified Data.Map
import qualified Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Prelude
import qualified GHC.Data.Graph.Directed

-- TODO: to qualified
import DataTypes
import Records

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
    GHC.Data.Graph.Directed.node_dependencies = [7, 10]
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

graph' ::
  [Icon] ->
  GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node Int Icon)
graph' inputIcons =
  GHC.Data.Graph.Directed.graphFromEdgedVerticesUniq nodes
  where
    nodes = [GHC.Data.Graph.Directed.DigraphNode {
        GHC.Data.Graph.Directed.node_payload = Records.Icon { iconKey = iconKeyValue, iconText = iconTextValue, iconType = iconTypeValue },
        GHC.Data.Graph.Directed.node_key = iconKeyValue,
        GHC.Data.Graph.Directed.node_dependencies = [] } | Records.Icon { iconKey = iconKeyValue, iconText = iconTextValue, iconType = iconTypeValue } <- inputIcons]

iconsWithKeys ::
  [Int] ->
  Data.Map.Map Int (GHC.Data.Graph.Directed.Node Int Icon) ->
  [GHC.Data.Graph.Directed.Node Int Icon]
iconsWithKeys ks = Data.Map.foldrWithKey (\k a acc -> if k `elem` ks then a:acc else acc) []

-- <- constructing the graph

-- graph manipulation ->

payload :: GHC.Data.Graph.Directed.Node Int Icon -> Icon
payload GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = x, GHC.Data.Graph.Directed.node_key = _, GHC.Data.Graph.Directed.node_dependencies = _ } = x

key :: GHC.Data.Graph.Directed.Node Int Icon -> Int
key GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = _, GHC.Data.Graph.Directed.node_key = x, GHC.Data.Graph.Directed.node_dependencies = _ } = x

dependencies :: GHC.Data.Graph.Directed.Node Int Icon -> [Int]
dependencies GHC.Data.Graph.Directed.DigraphNode { GHC.Data.Graph.Directed.node_payload = _, GHC.Data.Graph.Directed.node_key = _, GHC.Data.Graph.Directed.node_dependencies = x } = x

visualGraph ::
  GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node Int Icon) ->
  [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)]
visualGraph inputGraph = do
  let icons = Data.Map.fromList . map (\icon -> (key icon, icon)) $ GHC.Data.Graph.Directed.verticesG inputGraph
  let renderingOrder = titleIconKey
  let titleIcon = icons Data.Map.! titleIconKey
  let startingWidth = 0.0
  let startingDepth = 0.0
  let firstChildIconDepth = startingDepth + (-1.0) * cellHeight
  let Icon { iconText = titleIconText, iconType = _ } = payload titleIcon
  let titleIconDependenciesKeys = dependencies titleIcon
  let titleIconDependencies = iconsWithKeys titleIconDependenciesKeys icons
  let (_, _, childSubgraphVisualData) =
        visualSubgraph
          icons
          titleIconDependencies
          (renderingOrder + 1)
          startingWidth
          firstChildIconDepth
          startingWidth
          startingDepth

  (Diagrams.Prelude.p2 (0.0, 0.0),
    titleShape
      titleIconText
      renderingOrder
      (Diagrams.Prelude.p2 (startingWidth, startingDepth))) : childSubgraphVisualData

-- we should have a bit of new logic here:
-- if, while iterating through the list of icons, you come across an icon
-- that is already in the returned collection of triples (rendering order, max width, (coordinates, diagram))
-- take that found end icon from the returned collection and mappend a connection to the new parent node to it
visualSubgraph ::
  Data.Map.Map Int (GHC.Data.Graph.Directed.Node Int Icon) ->
  [GHC.Data.Graph.Directed.Node Int Icon] ->
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  (Int,
    Double,
    [(Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
      Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)])
visualSubgraph
  _
  []
  renderingOrder
  currentGraphWidth _ _ _ = (renderingOrder, currentGraphWidth, [])

visualSubgraph
  inputIcons
  [x]
  renderingOrder
  currentGraphWidth
  currentGraphDepth
  previousIconOriginCoordinateX
  previousIconOriginCoordinateY = do
    let (childSubgraphMaxUsedRenderingOrder,
          childSubgraphMaxUsedWidth,
          childSubgraphVisualData) =
            visualSubgraph
              inputIcons
              (iconsWithKeys (dependencies x) inputIcons)
              (renderingOrder + 1)
              currentGraphWidth
              (currentGraphDepth - cellHeight)
              currentGraphWidth
              currentGraphDepth

    (childSubgraphMaxUsedRenderingOrder,
      childSubgraphMaxUsedWidth,
      visualSubgraphNode
        currentGraphWidth
        currentGraphDepth
        [(previousIconOriginCoordinateX - currentGraphWidth,
          previousIconOriginCoordinateY - currentGraphDepth)]
        (payload x)
        renderingOrder
      : childSubgraphVisualData)

visualSubgraph
  inputIcons
  (x:xs)
  renderingOrder
  currentGraphWidth
  currentGraphDepth
  previousIconOriginCoordinateX
  previousIconOriginCoordinateY = do
    let (leftChildSubgraphMaxUsedRenderingOrder,
          leftChildSubgraphMaxUsedWidth,
          leftChildSubgraphVisualData) =
            visualSubgraph
              inputIcons
              (iconsWithKeys (dependencies x) inputIcons)
              (renderingOrder + 1)
              currentGraphWidth
              (currentGraphDepth - cellHeight)
              currentGraphWidth
              currentGraphDepth

    let newRightChildSubgraphWidth = leftChildSubgraphMaxUsedWidth + cellWidth

    let (rightChildSubgraphMaxUsedRenderingOrder,
          rightChildSubgraphMaxUsedWidth,
          rightChildSubgraphVisualData) =
            visualSubgraph
              inputIcons
              xs
              leftChildSubgraphMaxUsedRenderingOrder
              newRightChildSubgraphWidth
              currentGraphDepth
              currentGraphWidth
              (currentGraphDepth + cellHeight)

    (rightChildSubgraphMaxUsedRenderingOrder,
      rightChildSubgraphMaxUsedWidth,
      visualSubgraphNode
        currentGraphWidth
        currentGraphDepth
        [(previousIconOriginCoordinateX - currentGraphWidth,
          previousIconOriginCoordinateY - currentGraphDepth)]
        (payload x)
        renderingOrder
      : leftChildSubgraphVisualData ++ rightChildSubgraphVisualData)

-- this function generates a pair of:
-- * origin coordinates of a diagram (where the diagram should be positioned)
-- * that diagram
-- based on:
-- * the origin coordinates which are determined by the calling function
-- * a list of distances to parent diagrams
--   * some diagrams have multiple children
--   * some have multiple parents
-- * drakon icon to be translated into a diagram
-- * some troubleshooting information:
--   * rendering order of the current icon
--   * max graph width reached by any icon/diagram rendered before this one
visualSubgraphNode ::
  Double ->
  Double ->
  [(Double, Double)] ->
  Icon ->
  Int ->
  (Diagrams.Prelude.Point Diagrams.Prelude.V2 Double,
    Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B)
visualSubgraphNode
  currentGraphWidth
  currentGraphDepth
  distancesToOriginCoordinatesOfPreviousIcons
  Icon { iconText = x, iconType = y }
  renderingOrder = do
    let originCoordinates = Diagrams.Prelude.p2 (currentGraphWidth, currentGraphDepth)

    (originCoordinates,
      correctShape
        y
        distancesToOriginCoordinatesOfPreviousIcons
        x
        renderingOrder
        originCoordinates)

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
fontSize = 0.075

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

connectionToParentIcons ::
  [(Double, Double)] ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
connectionToParentIcons =
  foldl
    (\acc (vectorX, vectorY) ->
      acc
      <>
      connectionToParentIcon vectorX vectorY)
    mempty

correctShape ::
  IconType ->
  [(Double, Double)] ->
  String ->
  Int ->
  Diagrams.Prelude.Point Diagrams.Prelude.V2 Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
correctShape Title _ titleIconText renderingOrder originCoordinates =
  titleShape titleIconText renderingOrder originCoordinates
correctShape End distancesToOriginCoordinatesOfPreviousIcons endIconText renderingOrder originCoordinates =
  endShape distancesToOriginCoordinatesOfPreviousIcons endIconText renderingOrder originCoordinates
correctShape Question distancesToOriginCoordinatesOfPreviousIcons questionIconText renderingOrder originCoordinates =
  questionShape distancesToOriginCoordinatesOfPreviousIcons questionIconText renderingOrder originCoordinates
correctShape Action distancesToOriginCoordinatesOfPreviousIcons actionIconText renderingOrder originCoordinates =
  actionShape distancesToOriginCoordinatesOfPreviousIcons actionIconText renderingOrder originCoordinates

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
  Diagrams.Prelude.Point Diagrams.Prelude.V2 Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
titleShape
  titleIconText
  renderingOrder
  originCoordinates = do
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
      text ("origin coordinates: " ++ show originCoordinates) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> shape
  else
    text titleIconText 0.0 0.0
    <> shape

actionShape ::
  [(Double, Double)] ->
  String ->
  Int ->
  Diagrams.Prelude.Point Diagrams.Prelude.V2 Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
actionShape
  vectorsToParentCoordinates
  actionIconText
  renderingOrder
  originCoordinates = do
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
      text ("origin coordinates: " ++ show originCoordinates) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> shape
      <> connectionToParentIcons vectorsToParentCoordinates
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.ultraThin
  else
    text actionIconText 0.0 0.0
    <> shape
    <> connectionToParentIcons vectorsToParentCoordinates
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

questionShape ::
  [(Double, Double)] ->
  String ->
  Int ->
  Diagrams.Prelude.Point Diagrams.Prelude.V2 Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
questionShape
  vectorsToParentCoordinates
  questionIconText
  renderingOrder
  originCoordinates = do
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
      text ("origin coordinates: " ++ show originCoordinates) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> text "yes" (iconWidth * (-0.1)) (iconHeight * (-0.7))
      <> text "no" (iconWidth * 0.55) (iconHeight * 0.15)
      <> shape
      <> connectionToParentIcons vectorsToParentCoordinates
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.ultraThin
  else
    text questionIconText 0.0 0.0
    <> text "yes" (iconWidth * (-0.1)) (iconHeight * (-0.7))
    <> text "no" (iconWidth * 0.55) (iconHeight * 0.15)
    <> shape
    <> connectionToParentIcons vectorsToParentCoordinates
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.ultraThin

endShape ::
  [(Double, Double)] ->
  String ->
  Int ->
  Diagrams.Prelude.Point Diagrams.Prelude.V2 Double ->
  Diagrams.Prelude.Diagram Diagrams.Backend.SVG.CmdLine.B
endShape
  vectorsToParentCoordinates
  endIconText
  renderingOrder
  originCoordinates = do
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
      text ("origin coordinates: " ++ show originCoordinates) 0.0 0.0)
      Diagrams.Prelude.#
      Diagrams.Prelude.translate (Diagrams.Prelude.r2 (0, fontSize))
      <> shape
      <> connectionToParentIcons vectorsToParentCoordinates
      Diagrams.Prelude.#
      Diagrams.Prelude.lc lineColour
      Diagrams.Prelude.#
      Diagrams.Prelude.lw Diagrams.Prelude.ultraThin
  else
    text endIconText 0.0 0.0
    <> shape
    <> connectionToParentIcons vectorsToParentCoordinates
    Diagrams.Prelude.#
    Diagrams.Prelude.lc lineColour
    Diagrams.Prelude.#
    Diagrams.Prelude.lw Diagrams.Prelude.ultraThin