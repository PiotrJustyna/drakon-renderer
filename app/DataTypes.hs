{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Text
import qualified GHC.Utils.Outputable

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