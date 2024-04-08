module Main where

import GHC.Data.Graph.Directed

node1 :: Node Int String
node1 = DigraphNode { node_payload = "hello", node_key = 1, node_dependencies = [] }

main :: IO ()
main = putStrLn "Hello, Haskell!"
