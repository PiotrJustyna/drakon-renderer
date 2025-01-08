{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON, withText)
import Data.Text (unpack)

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
  | Choice

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
  (==) Choice Choice = True
  (==) Choice _ = False

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
  show Choice = "Choice"

instance ToJSON IconKind where
  toJSON Title = String "Title"
  toJSON End = String "End"
  toJSON Action = String "Action"
  toJSON Question = String "Question"
  toJSON Headline = String "Headline"
  toJSON Address = String "Address"
  toJSON ForStart = String "ForStart"
  toJSON ForEnd = String "ForEnd"
  toJSON ValentPoint = String "ValentPoint"
  toJSON Choice = String "Choice"

instance FromJSON IconKind where
  parseJSON =
    withText "iconKind" $ \t ->
      case unpack t of
        "Title" -> pure Title
        "End" -> pure End
        "Action" -> pure Action
        "Question" -> pure Question
        "Headline" -> pure Headline
        "Address" -> pure Address
        "ForStart" -> pure ForStart
        "ForEnd" -> pure ForEnd
        "ValentPoint" -> pure ValentPoint
        "Choice" -> pure Choice
        unknown -> fail $ "unknown iconKind: " <> unknown
