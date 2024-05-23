module Main where

import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr

import qualified Parser

import qualified System.IO

main ::
  IO ()
main = do
  GHC.Utils.Outputable.printSDocLn GHC.Utils.Outputable.defaultSDocContext GHC.Utils.Ppr.LeftMode System.IO.stdout $ GHC.Utils.Outputable.ppr $ Parser.parse Parser.iconDefinition "title t \"title - description\""
