{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
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
  let diagramFileName = "drakon-diagram-2"

  let inputFilePath = "./diagrams/" ++ diagramFileName ++ ".json"

  let outputFilePath = "./diagrams/" ++ diagramFileName ++ "-drakon-layout.json"

  fileSizeInBytes <- System.Directory.getFileSize inputFilePath

  if fileSizeInBytes > maxInputFileSizeInBytes
    then
      putStrLn $ "Problem with diagram file \"" ++ inputFilePath ++ "\" (" ++ show fileSizeInBytes ++ " bytes). Max allowed input file size: " ++ show maxInputFileSizeInBytes ++ " bytes."
    else do
      content <- Control.Exception.catch (Data.ByteString.Lazy.readFile inputFilePath) handleReadError

      case Data.Aeson.decode content :: Maybe [Records.Icon] of
        Just icons -> do
          let graph = Records.directedGraph icons

          GHC.Utils.Outputable.printSDocLn
            GHC.Utils.Outputable.defaultSDocContext
            GHC.Utils.Ppr.LeftMode
            System.IO.stdout . GHC.Utils.Outputable.ppr $ graph

          handle <- System.IO.openFile outputFilePath System.IO.WriteMode

          Data.ByteString.Lazy.hPutStr handle (Data.Aeson.Encode.Pretty.encodePretty $ LayoutEngine.cartesianPositioning graph)

          System.IO.hClose handle
        Nothing -> do
          let unpackedContent = Data.ByteString.Lazy.Char8.unpack content
          putStrLn $ "Problem interpreting diagram file \"" ++ inputFilePath ++ "\". Details: " ++ unpackedContent
