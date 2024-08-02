{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
import qualified Records
import qualified System.IO

directedGraph ::
  [Records.Icon] ->
  GHC.Data.Graph.Directed.Graph
    (GHC.Data.Graph.Directed.Node
    GHC.Data.FastString.FastString
    Records.Icon)
directedGraph icons =
  GHC.Data.Graph.Directed.graphFromEdgedVerticesUniq nodes
  where
    nodes = [GHC.Data.Graph.Directed.DigraphNode {
        GHC.Data.Graph.Directed.node_payload = icon,
        GHC.Data.Graph.Directed.node_key = GHC.Data.FastString.fsLit $ Records.getIconName icon,
        GHC.Data.Graph.Directed.node_dependencies = GHC.Data.FastString.fsLit <$> Records.getIconNamesOfDependentIcons icon }
        | icon <- icons]

main :: IO ()
main = do
  let filePath = "./diagrams/drakon-diagram-1.json"

  content <- Data.ByteString.Lazy.readFile filePath

  case Data.Aeson.decode content :: Maybe [Records.Icon] of
    Just icons -> do
      GHC.Utils.Outputable.printSDocLn
        GHC.Utils.Outputable.defaultSDocContext
        GHC.Utils.Ppr.LeftMode
        System.IO.stdout . GHC.Utils.Outputable.ppr $ directedGraph icons
    Nothing -> return ()
