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
  let titleIcon = Icon { iconText = "hello world process", iconType = Title }

  let actionIcon = Icon { iconText = "Hello, world!", iconType = Action }

  let endIcon = Icon { iconText = "end", iconType = End }

  let serializedIcons = Data.Aeson.encode [ titleIcon, actionIcon, endIcon ]

  print serializedIcons

  let icons = Data.Aeson.decode "[{\"iconText\":\"hello world process\",\"iconType\":\"Title\"},{\"iconText\":\"Hello, world!\",\"iconType\":\"Action\"},{\"iconText\":\"end\",\"iconType\":\"End\"}]" :: Maybe [Icon]
  print icons