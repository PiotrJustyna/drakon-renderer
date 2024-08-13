{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified GHC.Utils.Outputable
import qualified GHC.Utils.Ppr
import qualified LayoutEngine
import qualified Records
import qualified System.Directory
import qualified System.IO
import qualified Control.Exception

maxInputFileSizeInBytes :: Integer
maxInputFileSizeInBytes = 102400

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
          let graph = Records.directedGraph icons

          GHC.Utils.Outputable.printSDocLn
            GHC.Utils.Outputable.defaultSDocContext
            GHC.Utils.Ppr.LeftMode
            System.IO.stdout . GHC.Utils.Outputable.ppr $ graph

          print . LayoutEngine.cartesianPositioning $ graph
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem interpreting diagram file \"" ++ diagramFilePath ++ "\". Details: " ++ unpackedContent
