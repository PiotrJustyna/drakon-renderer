module Main where

import qualified Parser

main ::
  IO ()
main = do
  print $ Parser.parse Parser.iconDefinition "icon \"starticon1\" as start"
