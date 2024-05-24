module Main where

import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr

import qualified Parser

import qualified System.IO
import qualified System.Environment

inputValidation :: [String] -> (Bool, String)
inputValidation [] = (False, "no input provided")
inputValidation [x] = (True, x)
inputValidation inputList = (False, "only one input argument expected but " ++ show (length inputList) ++ " provided")

main ::
  IO ()
main = do
  input <- System.Environment.getArgs :: IO [String]

  case inputValidation input of
    (True, x) -> do
      putStrLn ("input: \"" ++ x ++ "\" translates to:")
      GHC.Utils.Outputable.printSDocLn
        GHC.Utils.Outputable.defaultSDocContext
        GHC.Utils.Ppr.LeftMode
        System.IO.stdout $ GHC.Utils.Outputable.ppr $ Parser.parse Parser.iconDefinition x
    (False, x) -> putStrLn x