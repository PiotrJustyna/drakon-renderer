module LayoutEngine (positionDependencyPlanes, positionIcons, positionIcons', reducedDependencyPlane, firstPath) where

import qualified Records

iconWidth :: Double
iconWidth = 1.0

iconHeight :: Double
iconHeight = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

firstPath :: Records.Icon -> [Records.Icon] -> [Records.Icon]
firstPath currentIcon allIcons =
    case Records.allDependents' currentIcon allIcons of
      [] -> []
      (d:ds) -> [d] ++ firstPath d allIcons

firstIconPositioned :: [Records.Icon] -> Double -> Double -> Maybe Records.PositionedIcon
firstIconPositioned [] _ _    = Nothing
firstIconPositioned (i:_) x y = Just $ Records.PositionedIcon {
  Records.icon = i,
  Records.iconPositionX = x,
  Records.iconPositionY = y }

positionDependencyPlanes :: [[Records.Icon]] -> [Records.PositionedIcon]
positionDependencyPlanes [] = []
positionDependencyPlanes dependencyPlane =
  column ++
  positionDependencyPlanes' newDependencyPlane (iconWidth + spaceBetweenIconsX) column
  where
    column = positionIcons dependencyPlane 0.0
    newDependencyPlane = reducedDependencyPlane dependencyPlane column

positionDependencyPlanes' :: [[Records.Icon]] -> Double -> [Records.PositionedIcon] -> [Records.PositionedIcon]
positionDependencyPlanes' [] _ _ = []
positionDependencyPlanes' dependencyPlane x positionedIcons =
  column ++
  positionDependencyPlanes' newDependencyPlane (x + iconWidth + spaceBetweenIconsX) (column ++ positionedIcons)
  where
    column = positionIcons' dependencyPlane x positionedIcons
    newDependencyPlane = reverse $ reducedDependencyPlane dependencyPlane column

positionIcons :: [[Records.Icon]] -> Double -> [Records.PositionedIcon]
positionIcons [] _ = []
positionIcons (currentRow:remainingRows) y =
  case firstIconPositioned currentRow 0.0 y of
    Nothing -> positionIcons remainingRows (y - iconHeight)
    Just x -> x:positionIcons remainingRows (y - iconHeight)

positionIcons' :: [[Records.Icon]] -> Double -> [Records.PositionedIcon] -> [Records.PositionedIcon]
positionIcons' dependencyPlane x currentlyPositionedIcons = newPositionedIcons
  where
    firstPositionedIcon = foldl (\acc iconsRow ->
      case iconsRow of
        [] -> acc
        icons ->
          case Records.lowestParentAndItsIcon icons currentlyPositionedIcons of
            Nothing -> acc
            Just (lowestParent, itsIcon) -> (toPositionedIconWithParent' itsIcon lowestParent x) : acc) ([] :: [Records.PositionedIcon]) dependencyPlane
    newPositionedIcons = case firstPositionedIcon  of
      [] -> []
      newParents ->
        case Records.lowest newParents of
          Nothing -> (head newParents) : firstLineDependents (head newParents) dependencyPlane
          Just lowestParent -> lowestParent : firstLineDependents lowestParent dependencyPlane

firstLineDependents :: Records.PositionedIcon -> [[Records.Icon]] -> [Records.PositionedIcon]
firstLineDependents parent dependencyPlane =
  case namesOfDependentIcons of
  [] -> []
  _ -> foldl (\acc iconsRow ->
    case iconsRow of
      [] -> acc
      (i:_) ->
        if Records.getIconName i `elem` namesOfDependentIcons then
          let newPositionedIcon = toPositionedIconWithParent i parent
          in newPositionedIcon : firstLineDependents newPositionedIcon dependencyPlane ++ acc
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
    y = Records.getPositionedIconPositionY parent - iconHeight

toPositionedIconWithParent' :: Records.Icon -> Records.PositionedIcon -> Double -> Records.PositionedIcon
toPositionedIconWithParent' icon parent x =
  Records.PositionedIcon {
    Records.icon = icon,
    Records.iconPositionX = x,
    Records.iconPositionY = y }
  where
    y = Records.getPositionedIconPositionY parent - iconHeight

reducedDependencyPlaneRow :: [Records.Icon] -> [Records.PositionedIcon] -> [[Records.Icon]]
reducedDependencyPlaneRow icons positionedIcons =
  case result of
    [] -> []
    _ -> [result]
  where
    result = filter (`Records.notIconElem` positionedIcons) icons

reducedDependencyPlane :: [[Records.Icon]] -> [Records.PositionedIcon] -> [[Records.Icon]]
reducedDependencyPlane dependencyPlane positionedIcons =
  foldl (\acc singleRow -> reducedDependencyPlaneRow singleRow positionedIcons ++ acc) [] dependencyPlane
