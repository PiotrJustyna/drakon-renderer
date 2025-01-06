module LayoutEngine
  ( dcPaths
  , balance
  , showBalancedPathsHeader
  , showBalancedPaths
  , positionIcons
  ) where

import qualified Data.Map
import qualified Records

iconWidth :: Double
iconWidth = 1.0

iconHeight :: Double
iconHeight = 1.0

spaceBetweenIconsX :: Double
spaceBetweenIconsX = 1.0

combine :: [Records.Icon] -> [Records.Icon] -> [[Records.Icon]]
combine parents = foldl (\acc dependent -> acc <> [dependent : parents]) []

pathsEqual :: [[Records.Icon]] -> [[Records.Icon]] -> Bool
pathsEqual x1 x2 =
  length x1 == length x2
    && foldl
         (\acc (x1', x2') -> length x1' == length x2' && null x1' || head x1' == head x2' && acc)
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
dcPaths :: [[Records.Icon]] -> [Records.Icon] -> [Records.Icon] -> [[Records.Icon]]
dcPaths paths allIcons convergenceIcons =
  let newPaths =
        foldl
          (\acc singleRow ->
             acc
               <> case Records.getDependentIconsWithBlacklist
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

showBalancedPathsHeader :: [[Records.Icon]] -> String
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

showBalancedPaths :: [[Records.Icon]] -> String
showBalancedPaths inputPaths =
  let maxColumnIndex = maximum $ map (\inputPath -> length inputPath - 1) inputPaths
      formatIcon row columnIndex =
        case drop columnIndex row of
          (icon:_) ->
            let name = Records.getIconName icon
                description = Records.getIconDescription icon
                dependents = show $ Records.getIconNamesOfDependentIcons icon
             in " **" <> name <> "** - " <> description <> " " <> dependents <> " |"
          [] -> " :negative_squared_cross_mark: |"
   in concat
        [ "|" <> concat [formatIcon row columnIndex | row <- inputPaths] <> "\n"
        | columnIndex <- [0 .. maxColumnIndex]
        ]

skipFirst :: [[Records.Icon]] -> [[Records.Icon]]
skipFirst = foldl (\acc row -> acc <> [drop 1 row]) []

takeFirst :: [[Records.Icon]] -> [[Records.Icon]]
takeFirst = foldl (\acc row -> acc <> [take 1 row]) []

iconPresent :: Records.Icon -> [[Records.Icon]] -> Bool
iconPresent x = any (elem x)

sliceMap :: [[Records.Icon]] -> Data.Map.Map Records.Icon Records.Icon
sliceMap [] = Data.Map.empty
sliceMap input =
  foldl
    (\acc row ->
       case row of
         (icon:_) ->
           let currentIconPresent = iconPresent icon (skipFirst input)
               newIcon =
                 if currentIconPresent
                   then Records.valentPoint "0" ":new:"
                   else icon
            in Data.Map.insertWith const icon newIcon acc
         [] -> acc)
    Data.Map.empty
    input

balanceFirstSlice :: [[Records.Icon]] -> Int -> ([[Records.Icon]], Int)
balanceFirstSlice [] nextAvailableValentPointName = ([], nextAvailableValentPointName)
balanceFirstSlice input nextAvailableValentPointName =
  let rowMap = sliceMap input
   in (if Data.Map.null rowMap
         then (input, nextAvailableValentPointName)
         else foldl
                (\acc row ->
                   case row of
                     [] -> (fst acc <> [[]], snd acc)
                     (key:rest) ->
                       let value = rowMap Data.Map.! key
                           newName = "v" <> show (snd acc)
                        in if key == value
                             then (fst acc <> [key : rest], snd acc)
                             else ( fst acc <> [Records.updateName value newName : (key : rest)]
                                  , snd acc + 1))
                ([], nextAvailableValentPointName)
                input)

balance :: [[Records.Icon]] -> Int -> [[Records.Icon]]
balance unbalancedPaths nextAvailableValentPointName =
  let firstSliceInformation = balanceFirstSlice unbalancedPaths nextAvailableValentPointName
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
     [Records.Icon]
  -> Double
  -> Data.Map.Map String Records.PositionedIcon
  -> Data.Map.Map String Records.PositionedIcon
positionIconsInRow row newXCoordinate positionedIcons =
  fst
    (foldl
       (\acc icon ->
          let newYCoordinate = snd acc - iconHeight
           in ( Data.Map.insertWith
                  unconst
                  (Records.getIconName icon)
                  (Records.toPositionedIcon icon newXCoordinate newYCoordinate)
                  (fst acc)
              , newYCoordinate))
       (positionedIcons, 0.0)
       row)

positionIcons :: [[Records.Icon]] -> [Records.PositionedIcon]
positionIcons paths =
  Data.Map.elems . fst
    $ foldl
        (\rowAccu row ->
           let newXCoordinate = snd rowAccu
            in ( positionIconsInRow row newXCoordinate (fst rowAccu)
               , newXCoordinate + iconWidth + spaceBetweenIconsX))
        (Data.Map.empty, 0.0)
        paths
