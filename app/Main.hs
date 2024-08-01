{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified DataTypes
import qualified Records
import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
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
  let titleIcon = Records.Icon {
    Records.iconName = "1",
    Records.iconDescription = "hello world process",
    Records.iconNamesOfDependentIcons = ["2"],
    Records.iconKind = DataTypes.Title }

  let actionIcon = Records.Icon {
    Records.iconName = "2",
    Records.iconDescription = "Hello, world!",
    Records.iconNamesOfDependentIcons = ["3"],
    Records.iconKind = DataTypes.Action }

  let endIcon = Records.Icon {
    Records.iconName = "3",
    Records.iconDescription = "end",
    Records.iconNamesOfDependentIcons = [],
    Records.iconKind = DataTypes.End }

  GHC.Utils.Outputable.printSDocLn
    GHC.Utils.Outputable.defaultSDocContext
    GHC.Utils.Ppr.LeftMode
    System.IO.stdout . GHC.Utils.Outputable.ppr $ directedGraph [titleIcon, actionIcon, endIcon]
