module LayoutEngine (cartesianPositioning) where

import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified Records

originYCoordinate :: Int
originYCoordinate = 0

cartesianPositioning :: GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon) -> [Records.PositionedIcon]
cartesianPositioning x =
  exploratoryCartesianPositioning originYCoordinate firstNode topologicallySortedNodes
  where
    topologicallySortedNodes = GHC.Data.Graph.Directed.topologicalSortG x
    firstNode = head topologicallySortedNodes

exploratoryCartesianPositioning :: Int -> GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon] -> [Records.PositionedIcon]
exploratoryCartesianPositioning y n ns =
  Records.PositionedIcon { Records.icon = Records.payload n, Records.iconPositionY = y } : concat [exploratoryCartesianPositioning (y - 1) dependentNode ns | dependentNode <- dependentNodes]
  where
    nodeKeysOfDependentNodes = Records.dependencies n
    dependentNodes = Records.nodesIdentifiedWithKeys ns nodeKeysOfDependentNodes