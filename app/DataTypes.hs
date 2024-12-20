{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import qualified Data.Aeson
import qualified Data.Text

data IconKind
  = Title
  | End
  | Action
  | Question
  | Headline
  | Address
  | ForStart
  | ForEnd
  | ValentPoint

instance Eq DataTypes.IconKind where
  (==) Title Title = True
  (==) Title _ = False
  (==) End End = True
  (==) End _ = False
  (==) Action Action = True
  (==) Action _ = False
  (==) Question Question = True
  (==) Question _ = False
  (==) Headline Headline = True
  (==) Headline _ = False
  (==) Address Address = True
  (==) Address _ = False
  (==) ValentPoint ValentPoint = True
  (==) ValentPoint _ = False
  (==) ForStart ForStart = True
  (==) ForStart _ = False
  (==) ForEnd ForEnd = True
  (==) ForEnd _ = False

instance Show IconKind where
  show Title = "Title"
  show End = "End"
  show Action = "Action"
  show Question = "Question"
  show Headline = "Headline"
  show Address = "Address"
  show ForStart = "ForStart"
  show ForEnd = "ForEnd"
  show ValentPoint = "ValentPoint"

instance Data.Aeson.ToJSON IconKind where
  toJSON Title = Data.Aeson.String "Title"
  toJSON End = Data.Aeson.String "End"
  toJSON Action = Data.Aeson.String "Action"
  toJSON Question = Data.Aeson.String "Question"
  toJSON Headline = Data.Aeson.String "Headline"
  toJSON Address = Data.Aeson.String "Address"
  toJSON ForStart = Data.Aeson.String "ForStart"
  toJSON ForEnd = Data.Aeson.String "ForEnd"
  toJSON ValentPoint = Data.Aeson.String "ValentPoint"

instance Data.Aeson.FromJSON IconKind where
  parseJSON =
    Data.Aeson.withText "iconKind" $ \t ->
      case Data.Text.unpack t of
        "Title" -> pure Title
        "End" -> pure End
        "Action" -> pure Action
        "Question" -> pure Question
        "Headline" -> pure Headline
        "Address" -> pure Address
        "ForStart" -> pure ForStart
        "ForEnd" -> pure ForEnd
        "ValentPoint" -> pure ValentPoint
        unknown -> fail $ "unknown iconKind: " <> unknown
