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

handleReadError ::
  Control.Exception.IOException ->
  IO Data.ByteString.Lazy.ByteString
handleReadError e = return . Data.ByteString.Lazy.Char8.pack $ "Error reading file: " ++ show e

main :: IO ()
main = do
  let diagramFilePath = "./diagrams/drakon-diagram-1.json"

  fileSizeInBytes <- System.Directory.getFileSize diagramFilePath

  if fileSizeInBytes > maxInputFileSizeInBytes
    then
      putStrLn $ "Problem with diagram file \"" ++ diagramFilePath ++ "\" (" ++ show fileSizeInBytes ++ " bytes). Max allowed input file size: " ++ show maxInputFileSizeInBytes ++ " bytes."
    else do
      content <- Control.Exception.catch (Data.ByteString.Lazy.readFile diagramFilePath) handleReadError

      case Data.Aeson.decode content :: Maybe [Records.Icon] of
        Just icons -> do
          GHC.Utils.Outputable.printSDocLn
            GHC.Utils.Outputable.defaultSDocContext
            GHC.Utils.Ppr.LeftMode
            System.IO.stdout . GHC.Utils.Outputable.ppr $ directedGraph icons
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem decoding diagram file \"" ++ diagramFilePath ++ "\". Details: " ++ unpackedContent
