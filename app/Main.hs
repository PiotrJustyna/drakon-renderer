{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Applicative
import qualified Data.Aeson

data Coord = Coord { x :: Double, y :: Double }
  deriving (Show)

instance Data.Aeson.ToJSON Coord where
  toJSON (Coord x' y') =
    Data.Aeson.object
    [
      "x" Data.Aeson..= x',
      "y" Data.Aeson..= y'
    ]

  toEncoding (Coord x' y') = Data.Aeson.pairs $
    "x" Data.Aeson..= x' <>
    "y" Data.Aeson..= y'

instance Data.Aeson.FromJSON Coord where
  parseJSON (Data.Aeson.Object v) = Coord <$>
                         v Data.Aeson..: "x" <*>
                         v Data.Aeson..: "y"
  parseJSON _          = Control.Applicative.empty

main :: IO ()
main = do
  let req = Data.Aeson.decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req