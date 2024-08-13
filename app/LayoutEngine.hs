module LayoutEngine (cartesianPositioning) where

import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified Records

originYCoordinate :: Int
originYCoordinate = 0

originXCoordinate :: Int
originXCoordinate = 0

cartesianPositioning :: GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon) -> [Records.PositionedIcon]
cartesianPositioning x =
  exploratoryCartesianPositioning originXCoordinate originYCoordinate firstNode topologicallySortedNodes
  where
    topologicallySortedNodes = GHC.Data.Graph.Directed.topologicalSortG x
    firstNode = head topologicallySortedNodes

exploratoryCartesianPositioning :: Int -> Int -> GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon] -> [Records.PositionedIcon]
exploratoryCartesianPositioning x y n ns =
  Records.PositionedIcon {
    Records.icon = Records.payload n,
    Records.iconPositionX = x,
    Records.iconPositionY = y } : concat [exploratoryCartesianPositioning (x + index) (y - 1) dependentNode ns | (index, dependentNode) <- indexedDependentNodes]
  where
    nodeKeysOfDependentNodes = Records.dependencies n
    dependentNodes = Records.nodesIdentifiedWithKeys ns nodeKeysOfDependentNodes
    indexedDependentNodes = zip [0 ..] dependentNodes :: [(Int, GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon)]