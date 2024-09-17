{-# LANGUAGE OverloadedStrings #-}

module Records where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified DataTypes
import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified Options.Applicative

--- DrakonRendererArguments -> --------------------------------------------------------------------

--- 2024-08-21 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- All the information the renderer needs to run.
---------------------------------------------------------------------------------------------------

data DrakonRendererArguments = DrakonRendererArguments
  { textInputPath   :: String,
    textOutputPath  :: String,
    svgOutputPath   :: String }

drakonRendererArguments :: Options.Applicative.Parser DrakonRendererArguments
drakonRendererArguments = DrakonRendererArguments
  <$> Options.Applicative.strOption
    ( Options.Applicative.long "textInputPath"
      <> Options.Applicative.short 'i'
      <> Options.Applicative.metavar "PATH"
      <> Options.Applicative.help "Path to input *.json drakon diagram file." )
  <*> Options.Applicative.strOption
    ( Options.Applicative.long "textOutputPath"
      <> Options.Applicative.short 't'
      <> Options.Applicative.metavar "PATH"
      <> Options.Applicative.help "Path to output *.json drakon diagram file." )
  <*> Options.Applicative.strOption
    ( Options.Applicative.long "svgOutputPath"
      <> Options.Applicative.short 's'
      <> Options.Applicative.metavar "PATH"
      <> Options.Applicative.help "Path to output *.svg drakon diagram file." )

--- <- DrakonRendererArguments --------------------------------------------------------------------



--- Icon -> ---------------------------------------------------------------------------------------

--- 2024-08-09 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- This is a basic representation of a drakon icon which, in its serialized form, is used as
--- renderer input.
---------------------------------------------------------------------------------------------------

data Icon = Icon {
  iconName                  :: String,
  iconDescription           :: String,
  iconNamesOfDependentIcons :: [String],
  iconKind                  :: DataTypes.IconKind }
    deriving (Show)

getIconName :: Icon -> String
getIconName Icon {
  iconName = x,
  iconDescription = _,
  iconNamesOfDependentIcons = _,
  iconKind = _ } = x

getIconDescription :: Icon -> String
getIconDescription Icon {
  iconName = _,
  iconDescription = x,
  iconNamesOfDependentIcons = _,
  iconKind = _ } = x

getIconNamesOfDependentIcons :: Icon -> [String]
getIconNamesOfDependentIcons Icon {
  iconName = _,
  iconDescription = _,
  iconNamesOfDependentIcons = x,
  iconKind = _ } = x

getIconNamesOfDependentIcons' :: Icon -> [Icon] -> [String]
getIconNamesOfDependentIcons' Icon {
  iconName = _,
  iconDescription = _,
  iconNamesOfDependentIcons = names,
  iconKind = _ } blacklist = case blacklist of
    []  -> names
    _   -> filter (\x -> all (\y -> x /= getIconName y) blacklist) names

getNumberOfDependentIcons :: Icon -> Int
getNumberOfDependentIcons Icon {
  iconName = _,
  iconDescription = _,
  iconNamesOfDependentIcons = x,
  iconKind = _ } = length x

getIconKind :: Icon -> DataTypes.IconKind
getIconKind Icon {
  iconName = _,
  iconDescription = _,
  iconNamesOfDependentIcons = _,
  iconKind = x } = x

instance GHC.Utils.Outputable.Outputable Icon where
    ppr = GHC.Utils.Outputable.text . show

instance Data.Aeson.ToJSON Icon where
  toJSON (Icon name description namesOfDependentIcons kind) =
    Data.Aeson.object [
      "iconName" Data.Aeson..= name,
      "iconDescription" Data.Aeson..= description,
      "iconNamesOfDependentIcons" Data.Aeson..= namesOfDependentIcons,
      "iconKind" Data.Aeson..= kind]

instance Data.Aeson.FromJSON Icon where
  parseJSON (Data.Aeson.Object v) =
    Icon <$>
      v Data.Aeson..: "iconName" <*>
      v Data.Aeson..: "iconDescription" <*>
      v Data.Aeson..: "iconNamesOfDependentIcons" <*>
      v Data.Aeson..: "iconKind"
  parseJSON _                     =
    Control.Applicative.empty

titleIcon :: [Icon] -> Icon
titleIcon allIcons = head $ filter (\x -> case getIconKind x of
  DataTypes.Title -> True
  _ -> False) allIcons

removeDuplicates :: [[Icon]] -> [Icon] -> [[Icon]]
removeDuplicates iconsBefore uniqueIcons = foldl (\singleRow -> [] iconsBefore

allDependents :: [Icon] -> [Icon] -> [[Icon]]
allDependents subset allIcons = case allDependentsOfAllDependents subset allIcons of
  [] -> []
  nextLevelDependents -> nextLevelDependents : allDependents nextLevelDependents allIcons

allDependentsOfAllDependents :: [Icon] -> [Icon] -> [Icon]
allDependentsOfAllDependents dependents allIcons = foldl (\acc singleDependent ->  acc ++ allDependentsOfOneDependent singleDependent allIcons acc) [] dependents

allDependentsOfOneDependent :: Icon -> [Icon] -> [Icon] -> [Icon]
allDependentsOfOneDependent icon allIcons butNotThese = dependents
  where
    dependentNames = getIconNamesOfDependentIcons' icon butNotThese
    dependents = filter
      (\singleIcon -> any (\singleDependentName -> singleDependentName == getIconName singleIcon) dependentNames)
      allIcons

directedGraph :: [Icon] -> GHC.Data.Graph.Directed.Graph (GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Icon)
directedGraph icons =
  GHC.Data.Graph.Directed.graphFromEdgedVerticesUniq nodes
  where
    nodes = [GHC.Data.Graph.Directed.DigraphNode {
        GHC.Data.Graph.Directed.node_payload = singleIcon,
        GHC.Data.Graph.Directed.node_key = GHC.Data.FastString.fsLit $ getIconName singleIcon,
        GHC.Data.Graph.Directed.node_dependencies = GHC.Data.FastString.fsLit <$> getIconNamesOfDependentIcons singleIcon }
        | singleIcon <- icons]

payload :: GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Icon -> Icon
payload
  GHC.Data.Graph.Directed.DigraphNode {
    GHC.Data.Graph.Directed.node_payload = i,
    GHC.Data.Graph.Directed.node_key = _,
    GHC.Data.Graph.Directed.node_dependencies = _ } = i

key :: GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Icon -> GHC.Data.FastString.FastString
key
  GHC.Data.Graph.Directed.DigraphNode {
    GHC.Data.Graph.Directed.node_payload = _,
    GHC.Data.Graph.Directed.node_key = k,
    GHC.Data.Graph.Directed.node_dependencies = _ } = k

dependencies :: GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Icon -> [GHC.Data.FastString.FastString]
dependencies
  GHC.Data.Graph.Directed.DigraphNode {
    GHC.Data.Graph.Directed.node_payload = _,
    GHC.Data.Graph.Directed.node_key = _,
    GHC.Data.Graph.Directed.node_dependencies = d } = d

nodesIdentifiedWithKeys :: [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Icon] -> [GHC.Data.FastString.FastString] -> [GHC.Data.Graph.Directed.Node GHC.Data.FastString.FastString Icon]
nodesIdentifiedWithKeys nodes = foldl (\acc x -> acc ++ filter (\y -> x == key y) nodes) []

--- <- Icon ---------------------------------------------------------------------------------------



--- Positioned Icon -> ----------------------------------------------------------------------------

--- 2024-08-09 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- This represents basic icon layout on a cartesian plane.
---------------------------------------------------------------------------------------------------
data PositionedIcon = PositionedIcon {
  icon              :: Icon,
  iconPositionX     :: Double,
  iconPositionY     :: Double }
    deriving (Show)

getPositionedIconName :: PositionedIcon -> String
getPositionedIconName PositionedIcon {
  icon = x,
  iconPositionX = _,
  iconPositionY = _ } = getIconName x

getPositionedIconPositionX :: PositionedIcon -> Double
getPositionedIconPositionX PositionedIcon {
  icon = _,
  iconPositionX = x,
  iconPositionY = _ } = x

getPositionedIconPositionY :: PositionedIcon -> Double
getPositionedIconPositionY PositionedIcon {
  icon = _,
  iconPositionX = _,
  iconPositionY = y } = y

getLastPositionedIconPositionX :: [PositionedIcon] -> Double
getLastPositionedIconPositionX x = case x of
  [] -> 0
  list -> getPositionedIconPositionX $ last list

getDependentPositionedIcons :: PositionedIcon -> [PositionedIcon] -> [PositionedIcon]
getDependentPositionedIcons PositionedIcon {
  icon = positionedIcon,
  iconPositionX = _,
  iconPositionY = _ } =
    filter (\x -> any (\y -> y == getPositionedIconName x) (getIconNamesOfDependentIcons positionedIcon))

instance Data.Aeson.ToJSON PositionedIcon where
  toJSON (PositionedIcon positionFreeIcon positionX positionY) =
    Data.Aeson.object [
      "icon" Data.Aeson..= positionFreeIcon,
      "iconPositionX" Data.Aeson..= positionX,
      "iconPositionY" Data.Aeson..= positionY]

--- <- Positioned Icon ----------------------------------------------------------------------------
