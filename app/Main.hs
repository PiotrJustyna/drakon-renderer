{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified DataTypes
import qualified Records

main :: IO ()
main = do
  let titleIcon = Records.Icon {
    Records.iconName = "1",
    Records.iconDescription = "hello world process",
    Records.iconKind = DataTypes.Title }

  let actionIcon = Records.Icon {
    Records.iconName = "2",
    Records.iconDescription = "Hello, world!",
    Records.iconKind = DataTypes.Action }

  let endIcon = Records.Icon {
    Records.iconName = "3",
    Records.iconDescription = "end",
    Records.iconKind = DataTypes.End }

  print [titleIcon, actionIcon, endIcon]