module LayoutEngine (positionDependencyPlanes, positionIcons, positionIcons', reducedDependencyPlane, firstPath, firstToContainUnpositionedDependents, firstPath', iconWidth, spaceBetweenIconsX, xyz) where

import qualified Records

iconWidth :: Double
iconWidth = 1.0

iconHeight :: Double
iconHeight = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

firstPath :: Records.PositionedIcon -> [Records.Icon] -> [Records.PositionedIcon]
firstPath parent allIcons =
  case Records.allDependents' parent allIcons of
    [] -> []
    (d:ds) -> (positionedDependentIcon d) : firstPath (positionedDependentIcon d) allIcons
  where
    x = Records.getPositionedIconPositionX parent
    y = Records.getPositionedIconPositionY parent
    positionedDependentIcon icon = Records.toPositionedIcon icon x (y - iconHeight)

firstPath' :: Records.PositionedIcon -> Double -> [Records.Icon] -> [Records.PositionedIcon] -> [Records.PositionedIcon]
firstPath' parent newX allIcons allPositionedIcons =
  case Records.allDependents' parent allIcons of
    [] -> []
    dependents -> case def dependents allPositionedIcons of
      Nothing -> []
      Just d -> (positionedDependentIcon d) : firstPath' (positionedDependentIcon d) newX allIcons allPositionedIcons
  where
    y = Records.getPositionedIconPositionY parent
    positionedDependentIcon icon = Records.toPositionedIcon icon newX (y - iconHeight)

def :: [Records.Icon] -> [Records.PositionedIcon] -> Maybe Records.Icon
def dependents allPositionedIcons =
  foldl (\acc x -> case acc of
    Nothing -> case abc (Records.getIconName x) allPositionedIcons of
      True -> Just x
      False -> acc
    _ -> acc) Nothing dependents

xyz :: [Records.PositionedIcon] -> [Records.Icon] -> Double -> [Records.PositionedIcon]
xyz positionedIcons icons newX = case firstToContainUnpositionedDependents positionedIcons of
  Nothing -> []
  Just parent ->
    let newDependents = (firstPath' parent newX icons positionedIcons)
    in newDependents ++ (xyz (positionedIcons ++ newDependents) icons (newX + iconWidth + spaceBetweenIconsX))

firstToContainUnpositionedDependents :: [Records.PositionedIcon] -> Maybe Records.PositionedIcon
firstToContainUnpositionedDependents positionedIcons =
  foldr (\singlePositionedIcon acc ->
    case acc of
      Nothing -> case containsUnpositionedDependents singlePositionedIcon positionedIcons of
        True -> Just singlePositionedIcon
        False -> Nothing
      a -> a) Nothing positionedIcons

containsUnpositionedDependents :: Records.PositionedIcon -> [Records.PositionedIcon] -> Bool
containsUnpositionedDependents x allPositionedIcons = case names of
  [] -> False
  otherwise -> any (\singleName -> abc singleName allPositionedIcons) names
  where
    names = Records.getPositionedIconNamesOfDependentIcons x

abc :: String -> [Records.PositionedIcon] -> Bool
abc iconName = all (\x -> iconName /= Records.getPositionedIconName x)

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
