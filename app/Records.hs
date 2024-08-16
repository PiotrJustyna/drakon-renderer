{-# LANGUAGE OverloadedStrings #-}

module Records where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified DataTypes
import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable

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

getIconNamesOfDependentIcons :: Icon -> [String]
getIconNamesOfDependentIcons Icon {
  iconName = _,
  iconDescription = _,
  iconNamesOfDependentIcons = x,
  iconKind = _ } = x

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
nodesIdentifiedWithKeys nodes keys = filter (\x -> any (\y -> y == key x) keys) nodes

--- <- Icon ---------------------------------------------------------------------------------------



--- Positioned Icon -> ----------------------------------------------------------------------------

--- 2024-08-09 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- This represents basic icon layout on a cartesian plane.
---------------------------------------------------------------------------------------------------

data PositionedIcon = PositionedIcon {
  icon              :: Icon,
  iconPositionX     :: Int,
  iconPositionY     :: Int }
    deriving (Show)

getPositionedIconName :: PositionedIcon -> String
getPositionedIconName PositionedIcon {
  icon = x,
  iconPositionX = _,
  iconPositionY = _ } = getIconName x

getPositionedIconPositionX :: PositionedIcon -> Int
getPositionedIconPositionX PositionedIcon {
  icon = _,
  iconPositionX = x,
  iconPositionY = _ } = x

getLastPositionedIconPositionX :: [PositionedIcon] -> Int
getLastPositionedIconPositionX x = case x of
  [] -> 0
  list -> getPositionedIconPositionX $ last list

collision :: PositionedIcon -> PositionedIcon -> Bool
collision
  PositionedIcon {
    icon = z1,
    iconPositionX = x1,
    iconPositionY = y1 }
  PositionedIcon {
    icon = z2,
    iconPositionX = x2,
    iconPositionY = y2 } =
      x1 == x2 &&
      y1 == y2 &&
      (getIconName z1) /= (getIconName z2)

instance Data.Aeson.ToJSON PositionedIcon where
  toJSON (PositionedIcon positionFreeIcon positionX positionY) =
    Data.Aeson.object [
      "icon" Data.Aeson..= positionFreeIcon,
      "iconPositionX" Data.Aeson..= positionX,
      "iconPositionY" Data.Aeson..= positionY]

--- <- Positioned Icon ----------------------------------------------------------------------------
