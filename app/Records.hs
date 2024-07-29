{-# LANGUAGE OverloadedStrings #-}

module Records where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified DataTypes

data Icon = Icon {
  iconName        :: String,
  iconDescription :: String,
  iconKind        :: DataTypes.IconKind }
    deriving (Show)

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