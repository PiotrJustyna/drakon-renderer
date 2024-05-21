module Main where

import qualified Parser
import qualified Renderer

main ::
  IO ()
main = do
  print $ Parser.parse Parser.naturalNumbers " [1, 22, 333, 4444, 55555] "
