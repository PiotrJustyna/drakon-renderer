module LayoutEngine (cartesianPositioning, abc, abc', extractPositionedIcons, spaceBetweenIconsX, iconWidth) where

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

abc :: [[Records.Icon]] -> Double -> [Records.PositionedIcon]
abc [] _ = []
abc (currentRow:remainingRows) y =
  case firstIconPositioned currentRow 0.0 y of
    Nothing -> abc remainingRows (y - iconHeight)
    Just x -> x : abc remainingRows (y - iconHeight)

-- todo1: y needs to change as we iterate
-- todo2: we only take the first found icon which parent is already a positioned icon
-- todo3: we also want all first dependents of the first found icon
abc' :: [[Records.Icon]] -> Double -> [Records.PositionedIcon] -> [Records.PositionedIcon]
abc' dependencyPlane x currentlyPositionedIcons = currentlyPositionedIcons ++ newPositionedIcons
  where
    firstPositionedIcon = foldl (\acc (i:_) ->
      case acc of
        Nothing ->
          case Records.iconParentElem i currentlyPositionedIcons of
            Nothing -> acc
            Just parent -> Just Records.PositionedIcon { Records.icon = i, Records.iconPositionX = x, Records.iconPositionY = (Records.getPositionedIconPositionY parent) - iconHeight }
        _ -> acc) (Nothing :: Maybe Records.PositionedIcon) dependencyPlane
    newPositionedIcons = case firstPositionedIcon  of
      Nothing -> []
      Just newParent -> newParent : (firstLineDependents newParent dependencyPlane)

firstLineDependents :: Records.PositionedIcon -> [[Records.Icon]] -> [Records.PositionedIcon]
firstLineDependents parent dependencyPlane =
  map
    (\i -> Records.PositionedIcon { Records.icon = i, Records.iconPositionX = x, Records.iconPositionY = y })
    (map head (filter (\(i:_) -> Records.getIconName i `elem` namesOfDependentIcons) dependencyPlane))
  where
    -- if any dependents found...
    namesOfDependentIcons = Records.getPositionedIconNamesOfDependentIcons parent
    x = Records.getPositionedIconPositionX parent
    y = (Records.getPositionedIconPositionY parent) - iconHeight

ghi :: [Records.Icon] -> [Records.PositionedIcon] -> [[Records.Icon]]
ghi icons positionedIcons =
  case result of
    [] -> []
    _ -> [result]
  where
    result = filter (\x -> Records.notIconElem x positionedIcons) icons

extractPositionedIcons :: [[Records.Icon]] -> [Records.PositionedIcon] -> [[Records.Icon]]
extractPositionedIcons dependencyPlane positionedIcons =
  foldl (\acc singleRow -> (ghi singleRow positionedIcons) ++ acc) [] dependencyPlane

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
