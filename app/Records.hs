{-# LANGUAGE OverloadedStrings #-}

module Records where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified DataTypes
import qualified GHC.Utils.Outputable

data Icon = Icon {
  iconKey :: Int,
  iconText :: String,
  iconType :: DataTypes.IconType }
    deriving (Show)

instance GHC.Utils.Outputable.Outputable Icon where
    ppr Icon {
      iconKey = iconKeyValue,
      iconText = iconTextValue,
      iconType = iconTypeValue } =
        GHC.Utils.Outputable.text $ show iconTypeValue ++ ": " ++ iconTextValue ++ " - " ++ show iconKeyValue

instance Data.Aeson.ToJSON Icon where
  toJSON (Icon iconKeyValue iconTextValue iconTypeValue) =
    Data.Aeson.object [
      "iconKey" Data.Aeson..= iconKeyValue,
      "iconText" Data.Aeson..= iconTextValue,
      "iconType" Data.Aeson..= iconTypeValue ]

instance Data.Aeson.FromJSON Icon where
  parseJSON (Data.Aeson.Object v) =
    Icon <$>
      v Data.Aeson..: "iconKey" <*>
      v Data.Aeson..: "iconText" <*>
      v Data.Aeson..: "iconType"
  parseJSON _                     =
    Control.Applicative.empty