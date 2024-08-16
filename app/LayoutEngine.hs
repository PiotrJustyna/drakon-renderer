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
 removeDuplicates $ exploratoryCartesianPositioning originXCoordinate originYCoordinate firstNode topologicallySortedNodes
  where
    topologicallySortedNodes = GHC.Data.Graph.Directed.topologicalSortG x
    firstNode = head topologicallySortedNodes

exploratoryCartesianPositioning :: Int -> Int -> GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon] -> [Records.PositionedIcon]
exploratoryCartesianPositioning x y n ns =
  [Records.PositionedIcon {
    Records.icon = Records.payload n,
    Records.iconPositionX = x,
    Records.iconPositionY = y }]
  where
    dependentNodes = Records.nodesIdentifiedWithKeys ns $ Records.dependencies n


























exploratoryCartesianPositioning' :: Int -> Int -> GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon] -> [Records.PositionedIcon]
exploratoryCartesianPositioning' x y n ns =
  Records.PositionedIcon {
    Records.icon = Records.payload n,
    Records.iconPositionX = x,
    Records.iconPositionY = y } : foldl (\acc (index, dependentNode) -> acc ++ exploratoryCartesianPositioning' (x + index) (y - 1) dependentNode ns) [] indexedDependentNodes
  where
    nodeKeysOfDependentNodes = Records.dependencies n
    dependentNodes = reverse $ Records.nodesIdentifiedWithKeys ns nodeKeysOfDependentNodes
    indexedDependentNodes = zip [0 ..] dependentNodes :: [(Int, GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon)]

removeDuplicates :: [Records.PositionedIcon] -> [Records.PositionedIcon]
removeDuplicates = reverse . foldl (\acc x -> if any (\y -> Records.getPositionedIconName y == Records.getPositionedIconName x) acc then acc else x:acc) []

-- pushPositionedIcons :: [Records.PositionedIcon] -> [Records.PositionedIcon]
-- pushPositionedIcons allPositionedIcons = foldr (\x acc -> if collidesWithAnotherIcon x allPositionedIcons then acc else x:acc) [] allPositionedIcons

-- collidesWithAnotherIcon :: Records.PositionedIcon -> [Records.PositionedIcon] -> Bool
-- collidesWithAnotherIcon singlePositonedIcon allPositionedIcons = any (\x -> Records.collision x singlePositonedIcon) allPositionedIcons