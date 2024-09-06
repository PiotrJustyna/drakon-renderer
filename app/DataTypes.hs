{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import qualified Data.Aeson
import qualified Data.Text

data IconKind = Title | End | Action | Question | Headline | Address

instance Show IconKind where
    show Title = "Title"
    show End = "End"
    show Action = "Action"
    show Question = "Question"
    show Headline = "Headline"
    show Address = "Address"

instance Data.Aeson.ToJSON IconKind where
  toJSON Title    = Data.Aeson.String "Title"
  toJSON End      = Data.Aeson.String "End"
  toJSON Action   = Data.Aeson.String "Action"
  toJSON Question = Data.Aeson.String "Question"
  toJSON Headline = Data.Aeson.String "Headline"
  toJSON Address = Data.Aeson.String "Address"

instance Data.Aeson.FromJSON IconKind where
  parseJSON = Data.Aeson.withText "iconKind" $ \t -> case Data.Text.unpack t of
    "Title"     -> pure Title
    "End"       -> pure End
    "Action"    -> pure Action
    "Question"  -> pure Question
    "Headline"  -> pure Headline
    "Address"  -> pure Address
    unknown -> fail $ "unknown iconKind: " <> unknown