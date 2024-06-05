module Main where

import qualified ParserV2

main ::
  IO ()
main = do
  -- Just ('a',"bc")
  print $ ParserV2.parse ParserV2.singleCharacterParser "abc"

  -- Just ('a',"bc")
  print $ ParserV2.parse
    (fmap (const 'A') ParserV2.singleCharacterParser)
    "abc"
