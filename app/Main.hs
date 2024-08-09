{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified GHC.Data.FastString
import qualified GHC.Data.Graph.Directed
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
import qualified Records
import qualified System.Directory
import qualified System.IO
import qualified Control.Exception

maxInputFileSizeInBytes :: Integer
maxInputFileSizeInBytes = 102400

originYCoordinate :: Int
originYCoordinate = 0

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

--- 2024-08-09 PJ:
--- If we place it in a module, it should be public
cartesianPositioning ::
  GHC.Data.Graph.Directed.Graph
    (GHC.Data.Graph.Directed.Node
    GHC.Data.FastString.FastString
    Records.Icon) ->
    [Records.PositionedIcon]
cartesianPositioning x =
  cartesianPositioningOfTopologicallySortedIcons originYCoordinate (GHC.Data.Graph.Directed.topologicalSortG x)

--- 2024-08-09 PJ:
--- If we place it in a module, it should be private
cartesianPositioningOfTopologicallySortedIcons ::
  Int ->
  [GHC.Data.Graph.Directed.Node
    GHC.Data.FastString.FastString
    Records.Icon] ->
  [Records.PositionedIcon]
cartesianPositioningOfTopologicallySortedIcons y (n : ns) =
  Records.PositionedIcon { Records.icon = i, Records.iconPositionY = y } : cartesianPositioningOfTopologicallySortedIcons (y - 1) ns
  where
    GHC.Data.Graph.Directed.DigraphNode {
      GHC.Data.Graph.Directed.node_payload = i,
      GHC.Data.Graph.Directed.node_key = _,
      GHC.Data.Graph.Directed.node_dependencies = _ } = n
cartesianPositioningOfTopologicallySortedIcons _ [] = []

handleReadError ::
  Control.Exception.IOException ->
  IO Data.ByteString.Lazy.ByteString
handleReadError e = return . Data.ByteString.Lazy.Char8.pack $ "Error reading file: " ++ show e

main :: IO ()
main = do
  let diagramFilePath = "./diagrams/drakon-diagram-2-unsorted.json"

  fileSizeInBytes <- System.Directory.getFileSize diagramFilePath

  if fileSizeInBytes > maxInputFileSizeInBytes
    then
      putStrLn $ "Problem with diagram file \"" ++ diagramFilePath ++ "\" (" ++ show fileSizeInBytes ++ " bytes). Max allowed input file size: " ++ show maxInputFileSizeInBytes ++ " bytes."
    else do
      content <- Control.Exception.catch (Data.ByteString.Lazy.readFile diagramFilePath) handleReadError

      case Data.Aeson.decode content :: Maybe [Records.Icon] of
        Just icons -> do
          let graph = directedGraph icons

          GHC.Utils.Outputable.printSDocLn
            GHC.Utils.Outputable.defaultSDocContext
            GHC.Utils.Ppr.LeftMode
            System.IO.stdout . GHC.Utils.Outputable.ppr $ graph

          print . cartesianPositioning $ graph
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem interpreting diagram file \"" ++ diagramFilePath ++ "\". Details: " ++ unpackedContent
