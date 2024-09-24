module LayoutEngine (cartesianPositioning, abc, abc', def, reducedDependencyPlane, spaceBetweenIconsX, iconWidth) where

import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified Records

originXCoordinate :: Double
originXCoordinate = 0.0

originYCoordinate :: Double
originYCoordinate = 0.0

iconWidth :: Double
iconWidth = 1.0

iconHeight :: Double
iconHeight = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

firstIconPositioned :: [Records.Icon] -> Double -> Double -> Maybe Records.PositionedIcon
firstIconPositioned [] _ _    = Nothing
firstIconPositioned (i:_) x y = Just $ Records.PositionedIcon {
  Records.icon = i,
  Records.iconPositionX = x,
  Records.iconPositionY = y }

def :: [[Records.Icon]] -> [Records.PositionedIcon]
def [] = []
def dependencyPlane = column ++ def' newDependencyPlane (iconWidth + spaceBetweenIconsX) column
  where
    column = abc dependencyPlane 0.0
    newDependencyPlane = reducedDependencyPlane dependencyPlane column

def' :: [[Records.Icon]] -> Double -> [Records.PositionedIcon] -> [Records.PositionedIcon]
def' [] _ _ = []
def' dependencyPlane x positionedIcons = column ++ def' newDependencyPlane (x + iconWidth + spaceBetweenIconsX) (column ++ positionedIcons)
  where
    column = abc' dependencyPlane x positionedIcons
    newDependencyPlane = reverse $ reducedDependencyPlane dependencyPlane column

abc :: [[Records.Icon]] -> Double -> [Records.PositionedIcon]
abc [] _ = []
abc (currentRow:remainingRows) y =
  case firstIconPositioned currentRow 0.0 y of
    Nothing -> abc remainingRows (y - iconHeight)
    Just x -> x:abc remainingRows (y - iconHeight)

abc' :: [[Records.Icon]] -> Double -> [Records.PositionedIcon] -> [Records.PositionedIcon]
abc' dependencyPlane x currentlyPositionedIcons = newPositionedIcons
  where
    firstPositionedIcon = foldl (\acc (i:_) ->
      case acc of
        Nothing ->
          case Records.iconParentElem i currentlyPositionedIcons of
            Nothing -> acc
            Just parent -> Just $ toPositionedIconWithParent' i parent x
        _ -> acc) (Nothing :: Maybe Records.PositionedIcon) dependencyPlane
    newPositionedIcons = case firstPositionedIcon  of
      Nothing -> []
      Just newParent -> newParent:(firstLineDependents newParent dependencyPlane)

firstLineDependents :: Records.PositionedIcon -> [[Records.Icon]] -> [Records.PositionedIcon]
firstLineDependents parent dependencyPlane =
  case namesOfDependentIcons of
  [] -> []
  _ -> foldl (\acc (i:_) ->
    if Records.getIconName i `elem` namesOfDependentIcons
      then
        let newPositionedIcon = toPositionedIconWithParent i parent
        in newPositionedIcon:(firstLineDependents newPositionedIcon dependencyPlane) ++ acc
      else acc) [] dependencyPlane
  where
    namesOfDependentIcons = Records.getPositionedIconNamesOfDependentIcons parent

toPositionedIconWithParent :: Records.Icon -> Records.PositionedIcon -> Records.PositionedIcon
toPositionedIconWithParent icon parent =
  Records.PositionedIcon {
    Records.icon = icon,
    Records.iconPositionX = x,
    Records.iconPositionY = y }
  where
    x = Records.getPositionedIconPositionX parent
    y = (Records.getPositionedIconPositionY parent) - iconHeight

toPositionedIconWithParent' :: Records.Icon -> Records.PositionedIcon -> Double -> Records.PositionedIcon
toPositionedIconWithParent' icon parent x =
  Records.PositionedIcon {
    Records.icon = icon,
    Records.iconPositionX = x,
    Records.iconPositionY = y }
  where
    y = (Records.getPositionedIconPositionY parent) - iconHeight

reducedDependencyPlaneRow :: [Records.Icon] -> [Records.PositionedIcon] -> [[Records.Icon]]
reducedDependencyPlaneRow icons positionedIcons =
  case result of
    [] -> []
    _ -> [result]
  where
    result = filter (\x -> Records.notIconElem x positionedIcons) icons

reducedDependencyPlane :: [[Records.Icon]] -> [Records.PositionedIcon] -> [[Records.Icon]]
reducedDependencyPlane dependencyPlane positionedIcons =
  foldl (\acc singleRow -> (reducedDependencyPlaneRow singleRow positionedIcons) ++ acc) [] dependencyPlane

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
    Records.iconPositionY = y }:positionedDependentIcons, Records.getLastPositionedIconPositionX positionedDependentIcons)
  where
    dependentNodes = Records.nodesIdentifiedWithKeys ns $ Records.dependencies n
    positionedDependentIcons = cartesianPositioningOfDependentNodes x (y - iconHeight) dependentNodes ns

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
