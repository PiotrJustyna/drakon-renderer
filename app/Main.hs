module Main where

import qualified ParserV2

main :: IO ()
main = do
  print . show $ ParserV2.parse (ParserV2.matchingCharacter 'h') "hello world"
  print . show $ ParserV2.parse (ParserV2.matchingString "hello") "hello world"
  print . show $ ParserV2.parse (ParserV2.symbol "hello") "    hello   world"