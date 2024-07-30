{-# LANGUAGE OverloadedStrings #-}

module Records where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified DataTypes
import qualified GHC.Utils.Outputable

data Icon = Icon {
  iconName        :: String,
  iconDescription :: String,
  iconKind        :: DataTypes.IconKind }
    deriving (Show)

instance GHC.Utils.Outputable.Outputable Icon where
    ppr icon = GHC.Utils.Outputable.text $ show icon

instance Data.Aeson.ToJSON Icon where
  toJSON (Icon name description kind) =
    Data.Aeson.object [
      "iconName" Data.Aeson..= name,
      "iconDescription" Data.Aeson..= description,
      "iconKind" Data.Aeson..= kind]

instance Data.Aeson.FromJSON Icon where
  parseJSON (Data.Aeson.Object v) =
    Icon <$>
      v Data.Aeson..: "iconName" <*>
      v Data.Aeson..: "iconDescription" <*>
      v Data.Aeson..: "iconKind"
  parseJSON _                     =
    Control.Applicative.empty