module LayoutEngine (cartesianPositioning, titleIcon, xyz) where

import qualified DataTypes
import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified Records

originXCoordinate :: Double
originXCoordinate = 0.0

originYCoordinate :: Double
originYCoordinate = 0.0

iconWidth :: Double
iconWidth = 1.0

iconHeigth :: Double
iconHeigth = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

titleIcon :: [Records.Icon] -> Records.Icon
titleIcon allIcons = head $ filter (\x -> case Records.getIconKind x of
  DataTypes.Title -> True
  _ -> False) allIcons

xyz :: [Records.Icon] -> [Records.Icon] -> [[Records.Icon]]
xyz subset allIcons = case abc subset allIcons of
  [] -> []
  nextLevelDependents -> nextLevelDependents : xyz nextLevelDependents allIcons

abc :: [Records.Icon] -> [Records.Icon] -> [Records.Icon]
abc dependents allIcons = theirDependents
  where
    theirDependents = foldl (\acc singleDependent ->  acc ++ ghi singleDependent allIcons acc) [] dependents

ghi :: Records.Icon -> [Records.Icon] -> [Records.Icon] -> [Records.Icon]
ghi icon allIcons butNotThese = dependents
  where
    dependentNames = Records.getIconNamesOfDependentIcons' icon butNotThese
    dependents = filter
      (\singleIcon -> any (\singleDependentName -> singleDependentName == Records.getIconName singleIcon) dependentNames)
      allIcons

cartesianPositioning :: GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon) -> [Records.PositionedIcon]
cartesianPositioning x =
  removeDuplicates . fst $ exploratoryCartesianPositioning originXCoordinate originYCoordinate firstNode topologicallySortedNodes
  where
    topologicallySortedNodes = GHC.Data.Graph.Directed.topologicalSortG x
    firstNode = head topologicallySortedNodes

exploratoryCartesianPositioning :: Double -> Double -> GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon] -> ([Records.PositionedIcon], Double)
exploratoryCartesianPositioning x y n ns =
  (Records.PositionedIcon {
    Records.icon = Records.payload n,
    Records.iconPositionX = x,
    Records.iconPositionY = y } : positionedDependentIcons, Records.getLastPositionedIconPositionX positionedDependentIcons)
  where
    dependentNodes = Records.nodesIdentifiedWithKeys ns $ Records.dependencies n
    positionedDependentIcons = cartesianPositioningOfDependentNodes x (y - iconHeigth) dependentNodes ns

cartesianPositioningOfDependentNodes :: Double -> Double -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon] -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Records.Icon] -> [Records.PositionedIcon]
cartesianPositioningOfDependentNodes _ _ [] _ = []
cartesianPositioningOfDependentNodes x y (d:ds) ns =
  positionedHeadIconAndItsDependentIcons ++ cartesianPositioningOfDependentNodes (maxX + iconWidth + spaceBetweenIconsX) y ds ns
  where
    result = exploratoryCartesianPositioning x y d ns
    positionedHeadIconAndItsDependentIcons = fst result
    maxX = snd result

removeDuplicates :: [Records.PositionedIcon] -> [Records.PositionedIcon]
removeDuplicates = reverse . foldl (\acc x -> if any (\y -> Records.getPositionedIconName y == Records.getPositionedIconName x) acc then acc else x:acc) []
