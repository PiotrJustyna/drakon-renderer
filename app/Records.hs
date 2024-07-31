{-# LANGUAGE OverloadedStrings #-}

module Records where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified DataTypes
import qualified GHC.Utils.Outputable

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

instance GHC.Utils.Outputable.Outputable Icon where
    ppr icon = GHC.Utils.Outputable.text $ show icon

instance Data.Aeson.ToJSON Icon where
  toJSON (Icon name description kind namesOfDependentIcons) =
    Data.Aeson.object [
      "iconName" Data.Aeson..= name,
      "iconDescription" Data.Aeson..= description,
      "iconKind" Data.Aeson..= kind,
      "iconNamesOfDependentIcons" Data.Aeson..= namesOfDependentIcons]

instance Data.Aeson.FromJSON Icon where
  parseJSON (Data.Aeson.Object v) =
    Icon <$>
      v Data.Aeson..: "iconName" <*>
      v Data.Aeson..: "iconDescription" <*>
      v Data.Aeson..: "iconKind" <*>
      v Data.Aeson..: "iconNamesOfDependentIcons"
  parseJSON _                     =
    Control.Applicative.empty