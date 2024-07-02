{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Text

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

data Icon = Icon { iconText :: String, iconType :: IconType }
  deriving (Show)

instance Data.Aeson.ToJSON Icon where
  toJSON (Icon iconText' iconType') = Data.Aeson.object [ "iconText" Data.Aeson..= iconText', "iconType" Data.Aeson..= iconType' ]

instance Data.Aeson.FromJSON Icon where
  parseJSON (Data.Aeson.Object v) = Icon <$> v Data.Aeson..: "iconText" <*> v Data.Aeson..: "iconType"
  parseJSON _                     = Control.Applicative.empty

main :: IO ()
main = do
  let icon = Icon { iconText = "text", iconType = Title }
  print icon

  let serializedIcon = Data.Aeson.encode icon
  print serializedIcon

  let icon' = Data.Aeson.decode serializedIcon :: Maybe Icon
  print icon'

  let problematicIcon = Data.Aeson.decode "{\"iconText\":\"text\",\"iconType\":\"Title\"}" :: Maybe Icon
  print problematicIcon