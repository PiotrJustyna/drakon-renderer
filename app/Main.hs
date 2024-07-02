{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Applicative
import qualified Data.Aeson

data Coord = Coord { x :: Double, y :: Double }
  deriving (Show)

instance Data.Aeson.ToJSON Coord where
  toJSON (Coord x' y') = Data.Aeson.object [ "x" Data.Aeson..= x', "y" Data.Aeson..= y' ]

  toEncoding (Coord x' y') = Data.Aeson.pairs $ "x" Data.Aeson..= x' <> "y" Data.Aeson..= y'

instance Data.Aeson.FromJSON Coord where
  parseJSON (Data.Aeson.Object v) = Coord <$> v Data.Aeson..: "x" <*> v Data.Aeson..: "y"
  parseJSON _                     = Control.Applicative.empty

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

data Icon = Icon { iconText :: String, iconType :: IconType }
  deriving (Show)

instance Data.Aeson.ToJSON Icon where
  toJSON (Icon iconText' iconType') = Data.Aeson.object [ "iconText" Data.Aeson..= iconText', "iconType" Data.Aeson..= iconType' ]

main :: IO ()
main = do
  let icon = Icon { iconText = "text", iconType = Title }
  print icon
  print $ Data.Aeson.encode icon
  let req = Data.Aeson.decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req