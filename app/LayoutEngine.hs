module LayoutEngine
  ( dcPaths
  , balance
  , showBalancedPathsHeader
  , showBalancedPaths
  , positionIcons
  ) where

import Data.Map (Map, (!), elems, empty, insertWith)
import Records
  ( Icon
  , PositionedIcon
  , getDependentIconsWithBlacklist
  , getIconDescription
  , getIconName
  , getIconNamesOfDependentIcons
  , toPositionedIcon
  , updateName
  , valentPoint
  )

iconWidth :: Double
iconWidth = 1.0

iconHeight :: Double
iconHeight = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

combine :: [Icon] -> [Icon] -> [[Icon]]
combine parents = foldl (\acc dependent -> acc <> [dependent : parents]) []

pathsEqual :: [[Icon]] -> [[Icon]] -> Bool
pathsEqual x1 x2 =
  length x1 == length x2
    && foldl
         (\acc (x1', x2') ->
            length x1' == length x2' && null x1' || head x1' == head x2' && acc)
         True
         (zip x1 x2)

-- "paths" are represented like this (simplified):
-- [
--     [Icon6, Icon3],
--     [Icon6, Icon4, Icon3]
-- ]
-- and they represent all paths starting at a given divergence icon
-- and ending at a given convergence icon.
-- d-c paths - my name for divergence icon -> convergence icon paths
-- all paths connecting
dcPaths :: [[Icon]] -> [Icon] -> [Icon] -> [[Icon]]
dcPaths paths allIcons convergenceIcons =
  let newPaths =
        foldl
          (\acc singleRow ->
             acc
               <> case getDependentIconsWithBlacklist
                         (head singleRow)
                         allIcons
                         convergenceIcons
                         singleRow of
                    [] -> [singleRow]
                    dependents -> combine singleRow dependents)
          []
          paths
   in if pathsEqual paths newPaths
        then map reverse paths
        else dcPaths newPaths allIcons convergenceIcons

showBalancedPathsHeader :: [[Icon]] -> String
showBalancedPathsHeader inputPaths =
  let header =
        foldl
          (\acc i ->
             case i of
               0 -> "\n| path " <> show (i + 1) <> " |"
               _ -> acc <> " path " <> show (i + 1) <> " |")
          ""
          [0 .. (length inputPaths - 1)]
      headerLineBreak =
        foldl
          (\acc i ->
             case i of
               0 -> "\n| --- |"
               _ -> acc <> " --- |")
          ""
          [0 .. (length inputPaths - 1)]
   in header <> headerLineBreak

showBalancedPaths :: [[Icon]] -> String
showBalancedPaths inputPaths =
  let maxColumnIndex =
        maximum $ map (\inputPath -> length inputPath - 1) inputPaths
      formatIcon row columnIndex =
        case drop columnIndex row of
          (icon:_) ->
            let name = getIconName icon
                description = getIconDescription icon
                dependents = show $ getIconNamesOfDependentIcons icon
             in " **"
                  <> name
                  <> "** - "
                  <> description
                  <> " "
                  <> dependents
                  <> " |"
          [] -> " :negative_squared_cross_mark: |"
   in concat
        [ "|" <> concat [formatIcon row columnIndex | row <- inputPaths] <> "\n"
        | columnIndex <- [0 .. maxColumnIndex]
        ]

skipFirst :: [[Icon]] -> [[Icon]]
skipFirst = foldl (\acc row -> acc <> [drop 1 row]) []

takeFirst :: [[Icon]] -> [[Icon]]
takeFirst = foldl (\acc row -> acc <> [take 1 row]) []

iconPresent :: Icon -> [[Icon]] -> Bool
iconPresent x = any (elem x)

sliceMap :: [[Icon]] -> Map Icon Icon
sliceMap [] = empty
sliceMap input =
  foldl
    (\acc row ->
       case row of
         (icon:_) ->
           let currentIconPresent = iconPresent icon (skipFirst input)
               newIcon =
                 if currentIconPresent
                   then valentPoint "0" ":new:"
                   else icon
            in insertWith const icon newIcon acc
         [] -> acc)
    empty
    input

balanceFirstSlice :: [[Icon]] -> Int -> ([[Icon]], Int)
balanceFirstSlice [] nextAvailableValentPointName =
  ([], nextAvailableValentPointName)
balanceFirstSlice input nextAvailableValentPointName =
  let rowMap = sliceMap input
   in (if null rowMap
         then (input, nextAvailableValentPointName)
         else foldl
                (\acc row ->
                   case row of
                     [] -> (fst acc <> [[]], snd acc)
                     (key:rest) ->
                       let value = rowMap ! key
                           newName = "v" <> show (snd acc)
                        in if key == value
                             then (fst acc <> [key : rest], snd acc)
                             else ( fst acc
                                      <> [ updateName value newName
                                             : (key : rest)
                                         ]
                                  , snd acc + 1))
                ([], nextAvailableValentPointName)
                input)

balance :: [[Icon]] -> Int -> [[Icon]]
balance unbalancedPaths nextAvailableValentPointName =
  let firstSliceInformation =
        balanceFirstSlice unbalancedPaths nextAvailableValentPointName
   in case fst firstSliceInformation of
        [] -> []
        result ->
          let firstBalancedSlice = takeFirst result
              remainingPaths = skipFirst result
           in if all null remainingPaths
                then firstBalancedSlice
                else zipWith
                       (<>)
                       firstBalancedSlice
                       (balance remainingPaths (snd firstSliceInformation))

unconst :: a -> a -> a
unconst _ x = x

positionIconsInRow ::
     [Icon] -> Double -> Map String PositionedIcon -> Map String PositionedIcon
positionIconsInRow row newXCoordinate positionedIcons =
  fst
    (foldl
       (\acc icon ->
          let newYCoordinate = snd acc - iconHeight
           in ( insertWith
                  unconst
                  (getIconName icon)
                  (toPositionedIcon icon newXCoordinate newYCoordinate)
                  (fst acc)
              , newYCoordinate))
       (positionedIcons, 0.0)
       row)

positionIcons :: [[Icon]] -> [PositionedIcon]
positionIcons paths =
  elems . fst
    $ foldl
        (\rowAccu row ->
           let newXCoordinate = snd rowAccu
            in ( positionIconsInRow row newXCoordinate (fst rowAccu)
               , newXCoordinate + iconWidth + spaceBetweenIconsX))
        (empty, 0.0)
        paths
