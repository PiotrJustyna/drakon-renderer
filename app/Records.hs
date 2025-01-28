{-# LANGUAGE OverloadedStrings #-}

module Records where

import Control.Applicative (empty)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(Object)
  , (.:)
  , (.=)
  , object
  , parseJSON
  , toJSON
  )
import Data.List (find)
import DataTypes (IconKind(..))
import Options.Applicative (Parser, help, long, metavar, short, strOption)

--- DrakonRendererArguments -> --------------------------------------------------------------------
--- 2024-08-21 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- All the information the renderer needs to run.
---------------------------------------------------------------------------------------------------
data DrakonRendererArguments = DrakonRendererArguments
  { inputPath :: String
  , layoutOutputPath :: String
  , balancedPathsOutputPath :: String
  , svgOutputPath :: String
  }

drakonRendererArguments :: Parser DrakonRendererArguments
drakonRendererArguments =
  DrakonRendererArguments
    <$> strOption
          (long "inputPath"
             <> short 'i'
             <> metavar "PATH"
             <> help "Path to input *.json drakon diagram file.")
    <*> strOption
          (long "layoutOutputPath"
             <> short 'l'
             <> metavar "PATH"
             <> help "Path to output *.json drakon diagram file.")
    <*> strOption
          (long "balancedPathsOutputPath"
             <> short 'b'
             <> metavar "PATH"
             <> help
                  "Path to output *.md file containing visual representation of diagram's balanced paths.")
    <*> strOption
          (long "svgOutputPath"
             <> short 's'
             <> metavar "PATH"
             <> help "Path to output *.svg drakon diagram file.")

--- <- DrakonRendererArguments --------------------------------------------------------------------
--- Icon -> ---------------------------------------------------------------------------------------
--- 2024-08-09 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- This is a basic representation of a drakon icon which, in its serialized form, is used as
--- renderer input.
---------------------------------------------------------------------------------------------------
data Icon = Icon
  { iconName :: String
  , iconDescription :: String
  , iconNamesOfDependentIcons :: [String]
  , iconKind :: IconKind
  } deriving (Show)

getIconName :: Icon -> String
getIconName Icon { iconName = x
                 , iconDescription = _
                 , iconNamesOfDependentIcons = _
                 , iconKind = _
                 } = x

getIconDescription :: Icon -> String
getIconDescription Icon { iconName = _
                        , iconDescription = x
                        , iconNamesOfDependentIcons = _
                        , iconKind = _
                        } = x

getIconNamesOfDependentIcons :: Icon -> [String]
getIconNamesOfDependentIcons Icon { iconName = _
                                  , iconDescription = _
                                  , iconNamesOfDependentIcons = x
                                  , iconKind = _
                                  } = x

getDependentIcons :: Icon -> [Icon] -> [Icon]
getDependentIcons Icon { iconName = _
                       , iconDescription = _
                       , iconNamesOfDependentIcons = namesOfDependentIcons
                       , iconKind = _
                       } allIcons =
  case namesOfDependentIcons of
    [] -> []
    xs -> filter (\singleIcon -> getIconName singleIcon `elem` xs) allIcons

getDependentIconsWithBlacklist :: Icon -> [Icon] -> [Icon] -> [Icon] -> [Icon]
getDependentIconsWithBlacklist parent allIcons parentBlacklist dependentBlacklist =
  if parent `elem` parentBlacklist
    then []
    else (case getIconNamesOfDependentIcons parent of
            [] -> []
            xs ->
              filter
                (\singleIcon -> getIconName singleIcon `elem` xs)
                nonBlacklistedDependents)
  where
    nonBlacklistedDependents = filter (`notElem` dependentBlacklist) allIcons

getIconNamesOfDependentIconsWithBlacklist :: Icon -> [Icon] -> [String]
getIconNamesOfDependentIconsWithBlacklist Icon { iconName = _
                                               , iconDescription = _
                                               , iconNamesOfDependentIcons = names
                                               , iconKind = _
                                               } blacklist =
  case blacklist of
    [] -> names
    _ -> filter (\x -> all (\y -> x /= getIconName y) blacklist) names

getNumberOfDependentIcons :: Icon -> Int
getNumberOfDependentIcons Icon { iconName = _
                               , iconDescription = _
                               , iconNamesOfDependentIcons = x
                               , iconKind = _
                               } = length x

getIconKind :: Icon -> IconKind
getIconKind Icon { iconName = _
                 , iconDescription = _
                 , iconNamesOfDependentIcons = _
                 , iconKind = x
                 } = x

updateName :: Icon -> String -> Icon
updateName x newName = x {iconName = newName}

addDependentName :: Icon -> String -> Icon
addDependentName Icon { iconName = name
                      , iconDescription = description
                      , iconNamesOfDependentIcons = dependentNames
                      , iconKind = kind
                      } newDependentName =
  Icon
    { iconName = name
    , iconDescription = description
    , iconNamesOfDependentIcons = newDependentName : dependentNames
    , iconKind = kind
    }

removeDependents :: Icon -> Icon
removeDependents x = x {iconNamesOfDependentIcons = []}

updateDependent :: Icon -> String -> String -> Icon
updateDependent Icon { iconName = name
                     , iconDescription = description
                     , iconNamesOfDependentIcons = dependents
                     , iconKind = kind
                     } oldDependentName newDependentName =
  Icon
    { iconName = name
    , iconDescription = description
    , iconNamesOfDependentIcons =
        foldl
          (\acc x ->
             acc
               <> [ if x == oldDependentName
                      then newDependentName
                      else x
                  ])
          []
          dependents
    , iconKind = kind
    }

valentPoint :: String -> String -> Icon
valentPoint name description =
  Icon
    { iconName = name
    , iconDescription = description
    , iconNamesOfDependentIcons = []
    , iconKind = ValentPoint
    }

instance ToJSON Icon where
  toJSON (Icon name description namesOfDependentIcons kind) =
    object
      [ "iconName" .= name
      , "iconDescription" .= description
      , "iconNamesOfDependentIcons" .= namesOfDependentIcons
      , "iconKind" .= kind
      ]

instance FromJSON Icon where
  parseJSON (Object v) =
    Icon
      <$> v .: "iconName"
      <*> v .: "iconDescription"
      <*> v .: "iconNamesOfDependentIcons"
      <*> v .: "iconKind"
  parseJSON _ = empty

instance Eq Icon where
  (==) (Icon name1 _ _ _) (Icon name2 _ _ _) = name1 == name2

instance Ord Icon where
  compare (Icon name1 _ _ _) (Icon name2 _ _ _) = compare name1 name2

findTitleIcon :: [Icon] -> Maybe Icon
findTitleIcon =
  find
    (\x ->
       case getIconKind x of
         Title -> True
         _ -> False)

findEndIcon :: [Icon] -> Maybe Icon
findEndIcon =
  find
    (\x ->
       case getIconKind x of
         End -> True
         _ -> False)

removeDuplicates :: [[Icon]] -> [Icon] -> [[Icon]]
removeDuplicates [] _ = []
removeDuplicates (singleRow:remainingRows) uniqueIcons =
  removeDuplicates remainingRows (uniqueIcons <> newUniqueIcons)
    <> [newUniqueIcons]
  where
    newUniqueIcons =
      foldl
        (\acc x ->
           if x `notElem` uniqueIcons
             then x : acc
             else acc)
        []
        singleRow

allDependents' :: PositionedIcon -> [Icon] -> [Icon]
allDependents' parent allIcons =
  foldl
    (\acc x ->
       case find (\singleIcon -> x == getIconName singleIcon) allIcons of
         Nothing -> acc
         Just dependentIcon -> acc <> [dependentIcon])
    []
    dependentNames
  where
    dependentNames = getIconNamesOfDependentIcons $ getIcon parent

allDependents :: [Icon] -> [Icon] -> [[Icon]]
allDependents subset allIcons =
  case allDependentsOfAllDependents subset allIcons of
    [] -> []
    nextLevelDependents ->
      nextLevelDependents : allDependents nextLevelDependents allIcons

allDependentsOfAllDependents :: [Icon] -> [Icon] -> [Icon]
allDependentsOfAllDependents dependents allIcons =
  foldl
    (\acc singleDependent ->
       acc <> allDependentsOfOneDependent singleDependent allIcons acc)
    []
    dependents

allDependentsOfOneDependent :: Icon -> [Icon] -> [Icon] -> [Icon]
allDependentsOfOneDependent parent allIcons butNotThese = dependents
  where
    dependentNames =
      getIconNamesOfDependentIconsWithBlacklist parent butNotThese
    dependents =
      reverse
        $ filter
            (\singleIcon ->
               any
                 (\singleDependentName ->
                    singleDependentName == getIconName singleIcon)
                 dependentNames)
            allIcons

--- <- Icon ---------------------------------------------------------------------------------------
--- Positioned Icon -> ----------------------------------------------------------------------------
--- 2024-08-09 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- This represents basic icon layout on a cartesian plane.
---------------------------------------------------------------------------------------------------
data PositionedIcon = PositionedIcon
  { icon :: Icon
  , iconPositionX :: Double
  , iconPositionY :: Double
  } deriving (Show)

getIcon :: PositionedIcon -> Icon
getIcon PositionedIcon {icon = x, iconPositionX = _, iconPositionY = _} = x

getPositionedIconName :: PositionedIcon -> String
getPositionedIconName PositionedIcon { icon = x
                                     , iconPositionX = _
                                     , iconPositionY = _
                                     } = getIconName x

getPositionedIconPositionX :: PositionedIcon -> Double
getPositionedIconPositionX PositionedIcon { icon = _
                                          , iconPositionX = x
                                          , iconPositionY = _
                                          } = x

getPositionedIconPositionY :: PositionedIcon -> Double
getPositionedIconPositionY PositionedIcon { icon = _
                                          , iconPositionX = _
                                          , iconPositionY = y
                                          } = y

getPositionedIconNamesOfDependentIcons :: PositionedIcon -> [String]
getPositionedIconNamesOfDependentIcons PositionedIcon { icon = x
                                                      , iconPositionX = _
                                                      , iconPositionY = _
                                                      } =
  getIconNamesOfDependentIcons x

getLastPositionedIconPositionX :: [PositionedIcon] -> Double
getLastPositionedIconPositionX x =
  case x of
    [] -> 0
    list -> getPositionedIconPositionX $ last list

getDependentPositionedIcons ::
     PositionedIcon -> [PositionedIcon] -> [PositionedIcon]
getDependentPositionedIcons PositionedIcon { icon = positionedIcon
                                           , iconPositionX = _
                                           , iconPositionY = _
                                           } =
  filter
    (\x ->
       any
         (\y -> y == getPositionedIconName x)
         (getIconNamesOfDependentIcons positionedIcon))

iconParentElem :: Icon -> [PositionedIcon] -> Maybe PositionedIcon
iconParentElem childIcon =
  find
    (\x -> getIconName childIcon `elem` getPositionedIconNamesOfDependentIcons x)

lowestParentAndItsIcon ::
     [Icon] -> [PositionedIcon] -> Maybe (PositionedIcon, Icon)
lowestParentAndItsIcon childrenIcons positionedIcons =
  foldl
    (\acc x ->
       case iconParentElem x positionedIcons of
         Nothing -> acc
         Just parent ->
           case acc of
             Nothing -> Just (parent, x)
             Just (currentLowestParent, itsIcon) ->
               if getPositionedIconPositionY parent
                    < getPositionedIconPositionY currentLowestParent
                 then Just (parent, x)
                 else Just (currentLowestParent, itsIcon))
    Nothing
    childrenIcons

notIconElem :: Icon -> [PositionedIcon] -> Bool
notIconElem x = all (\y -> getPositionedIconName y /= getIconName x)

lowest :: [PositionedIcon] -> Maybe PositionedIcon
lowest =
  foldl
    (\acc x ->
       case acc of
         Nothing -> Just x
         Just currentLowest ->
           Just
             $ if getPositionedIconPositionY x
                    < getPositionedIconPositionY currentLowest
                 then x
                 else currentLowest)
    Nothing

toPositionedIcon :: Icon -> Double -> Double -> PositionedIcon
toPositionedIcon i x y =
  PositionedIcon {icon = i, iconPositionX = x, iconPositionY = y}

instance ToJSON PositionedIcon where
  toJSON (PositionedIcon positionFreeIcon positionX positionY) =
    object
      [ "icon" .= positionFreeIcon
      , "iconPositionX" .= positionX
      , "iconPositionY" .= positionY
      ]
--- <- Positioned Icon ----------------------------------------------------------------------------
