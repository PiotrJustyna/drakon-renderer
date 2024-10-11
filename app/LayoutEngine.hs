module LayoutEngine (firstPaths) where

import qualified Records

iconWidth :: Double
iconWidth = 1.0

iconHeight :: Double
iconHeight = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

firstPaths :: Records.Icon -> [Records.Icon] -> [Records.PositionedIcon]
firstPaths titleIcon allIcons =
  positionedIcons ++ nextColumn positionedIcons allIcons 0.0
  where
    positionedTitleIcon = Records.toPositionedIcon titleIcon 0.0 0.0
    titleIconDependents = firstPath positionedTitleIcon allIcons
    positionedIcons  = positionedTitleIcon : titleIconDependents

firstPath :: Records.PositionedIcon -> [Records.Icon] -> [Records.PositionedIcon]
firstPath parent allIcons =
  case Records.allDependents' parent allIcons of
    [] -> []
    (d:_) -> positionedDependentIcon d : firstPath (positionedDependentIcon d) allIcons
  where
    x = Records.getPositionedIconPositionX parent
    y = Records.getPositionedIconPositionY parent
    positionedDependentIcon icon = Records.toPositionedIcon icon x (y - iconHeight)

firstPath' :: Records.PositionedIcon -> Double -> [Records.Icon] -> [Records.PositionedIcon] -> [Records.PositionedIcon]
firstPath' parent newX allIcons allPositionedIcons =
  case Records.allDependents' parent allIcons of
    [] -> []
    dependents -> case firstUnpositionedDependent dependents allPositionedIcons of
      (_, Nothing) -> []
      (index, Just d) ->
        positionedDependentIcon d index : firstPath' (positionedDependentIcon d index) (updatedNewX index) allIcons allPositionedIcons
  where
    y = Records.getPositionedIconPositionY parent
    updatedNewX index = newX + (index * (iconWidth + spaceBetweenIconsX))
    positionedDependentIcon icon index = Records.toPositionedIcon icon (updatedNewX index) (y - iconHeight)

firstUnpositionedDependent :: [Records.Icon] -> [Records.PositionedIcon] -> (Double, Maybe Records.Icon)
firstUnpositionedDependent dependents allPositionedIcons =
  foldl (\acc x -> case acc of
    (index, Nothing) ->
      (if doesNotContainName (Records.getIconName x) allPositionedIcons
        then (index, Just x)
        else (index + 1.0, Nothing))
    _ -> acc) (0.0, Nothing) dependents

nextColumn :: [Records.PositionedIcon] -> [Records.Icon] -> Double -> [Records.PositionedIcon]
nextColumn positionedIcons icons newX = case firstToContainUnpositionedDependents positionedIcons of
  Nothing -> []
  Just parent ->
    let newDependents = firstPath' parent newX icons positionedIcons
    in newDependents ++ nextColumn (positionedIcons ++ newDependents) icons (maxX newDependents)

maxX :: [Records.PositionedIcon] -> Double
maxX = foldl (\acc singlePositionedIcon -> max (Records.getPositionedIconPositionX singlePositionedIcon) acc) 0.0

firstToContainUnpositionedDependents :: [Records.PositionedIcon] -> Maybe Records.PositionedIcon
firstToContainUnpositionedDependents positionedIcons =
  foldr (\x acc ->
    case acc of
      Nothing -> (if containsUnpositionedDependents x positionedIcons then Just x else Nothing)
      a -> a) Nothing positionedIcons

containsUnpositionedDependents :: Records.PositionedIcon -> [Records.PositionedIcon] -> Bool
containsUnpositionedDependents x allPositionedIcons = case names of
  [] -> False
  _ -> any (`doesNotContainName` allPositionedIcons) names
  where
    names = Records.getPositionedIconNamesOfDependentIcons x

doesNotContainName :: String -> [Records.PositionedIcon] -> Bool
doesNotContainName iconName = all (\x -> iconName /= Records.getPositionedIconName x)