{-# LANGUAGE OverloadedStrings #-}

module Records where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified DataTypes
import qualified GHC.Utils.Outputable

--- Icon -> ---------------------------------------------------------------------------------------

--- 2024-08-09 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- This is a basic representation of a drakon icon and, in its serialized form, it is used as
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

--- <- Icon ---------------------------------------------------------------------------------------



--- Positioned Icon -> ----------------------------------------------------------------------------

--- 2024-08-09 PJ: --------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
--- This is an experimental attempt at representing basic icon layout on a cartesian plane.
---------------------------------------------------------------------------------------------------

data PositionedIcon = PositionedIcon {
  icon          :: Icon,
  iconPositionY :: Int }
    deriving (Show)

--- <- Positioned Icon ----------------------------------------------------------------------------
