{-# LANGUAGE OverloadedStrings #-}

module Icon where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Utils.Outputable

-- 2024-07-03 PJ:
-- --------------
-- IconType

data IconType = Title | End | Action | Question

instance Show IconType where
    show Title = "Title"
    show End = "End"
    show Action = "Action"
    show Question = "Question"

instance Data.Aeson.ToJSON IconType where
  toJSON Title    = Data.Aeson.String "Title"
  toJSON End      = Data.Aeson.String "End"
  toJSON Action   = Data.Aeson.String "Action"
  toJSON Question = Data.Aeson.String "Question"

instance Data.Aeson.FromJSON IconType where
  parseJSON = Data.Aeson.withText "iconType" $ \t -> case Data.Text.unpack t of
    "Title"     -> pure Title
    "End"       -> pure End
    "Action"    -> pure Action
    "Question"  -> pure Question
    unknown -> fail $ "unknown iconType: " <> unknown

-- 2024-07-03 PJ:
-- --------------
-- Icon

data Icon = Icon { iconText :: String, iconType :: IconType }
  deriving (Show)

instance GHC.Utils.Outputable.Outputable Icon where
    ppr Icon { iconText = x, iconType = y } = GHC.Utils.Outputable.text $ show y ++ ": " ++ x

instance Data.Aeson.ToJSON Icon where
  toJSON (Icon iconText' iconType') = Data.Aeson.object [ "iconText" Data.Aeson..= iconText', "iconType" Data.Aeson..= iconType' ]

instance Data.Aeson.FromJSON Icon where
  parseJSON (Data.Aeson.Object v) = Icon <$> v Data.Aeson..: "iconText" <*> v Data.Aeson..: "iconType"
  parseJSON _                     = Control.Applicative.empty