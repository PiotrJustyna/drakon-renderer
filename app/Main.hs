{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, catch)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString, hPutStr, readFile)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import DataTypes (IconKind(..))
import Diagrams.Backend.SVG (renderSVG')
import LayoutEngine
  ( balance
  , dcPaths
  , positionIcons
  , showBalancedPaths
  , showBalancedPathsHeader
  )
import Options.Applicative
  ( (<**>)
  , execParser
  , fullDesc
  , header
  , helper
  , info
  , progDesc
  )
import Records
  ( DrakonRendererArguments(DrakonRendererArguments)
  , Icon
  , drakonRendererArguments
  , findEndIcon
  , findTitleIcon
  , getIconKind
  , getIconName
  , getNumberOfDependentIcons
  )
import Renderer (renderAllConnections, renderAllIcons, svgOptions)
import System.Directory (getFileSize)
import System.IO (IOMode(WriteMode), hClose, openFile)

maxInputFileSizeInBytes :: Integer
maxInputFileSizeInBytes = 102400

handleReadError :: IOException -> IO ByteString
handleReadError e = return . pack $ "Error reading file: " <> show e

main :: IO ()
main = process =<< execParser options
  where
    options =
      info
        (drakonRendererArguments <**> helper)
        (fullDesc <> progDesc "drakon renderer" <> header "drakon renderer")

correctNumberOfQuestionDependencies :: Int
correctNumberOfQuestionDependencies = 2

oneTitleIconPresent :: [Icon] -> Maybe (String, String)
oneTitleIconPresent icons =
  if (1 :: Int)
       == foldl
            (\acc x ->
               case getIconKind x of
                 Title -> acc + 1
                 _ -> acc)
            0
            icons
    then Nothing
    else Just
           ( "Diagram is required to have exactly one icon of kind \""
               <> show Title
               <> "\"."
           , "Make sure your input diagram contains an icon of kind \""
               <> show Title
               <> "\" and that it is the only icon of that kind.")

oneEndIconPresent :: [Icon] -> Maybe (String, String)
oneEndIconPresent icons =
  if (1 :: Int)
       == foldl
            (\acc x ->
               case getIconKind x of
                 End -> acc + 1
                 _ -> acc)
            0
            icons
    then Nothing
    else Just
           ( "Diagram is required to have exactly one icon of kind \""
               <> show End
               <> "\"."
           , "Make sure your input diagram contains an icon of kind \""
               <> show End
               <> "\" and that it is the only icon of that kind.")

-- 2024-09-06 PJ:
-----------------
-- TODO: adjust for the new kinds of icons (headline + address).
correctNumberOfDependencies :: [Icon] -> Maybe (String, String)
correctNumberOfDependencies icons =
  case iconsWithIncorrectDependencies of
    [] -> Nothing
    _ ->
      Just
        ( take (length errorText - 2) errorText <> "."
        , "Make sure your icons have the expected number of dependencies. For reference: \""
            <> show Title
            <> "\" and \""
            <> show Action
            <> "\" icons should have 1 depdenency, \""
            <> show Question
            <> "\" icon should have 2 dependencies and \""
            <> show End
            <> "\" should have no dependencies.")
  where
    iconsWithIncorrectDependencies =
      foldl
        (\acc x ->
           case getIconKind x of
             Question ->
               if correctNumberOfQuestionDependencies
                    /= getNumberOfDependentIcons x
                 then getIconName x : acc
                 else acc
             End ->
               if 0 /= getNumberOfDependentIcons x
                 then getIconName x : acc
                 else acc
             _ ->
               if 1 /= getNumberOfDependentIcons x
                 then getIconName x : acc
                 else acc)
        []
        icons
    errorText =
      foldl
        (\acc x -> acc <> "\"" <> x <> "\", ")
        "Icons identified with following names contain incorrect number of dependencies: "
        iconsWithIncorrectDependencies

validation :: [[Icon] -> Maybe (String, String)] -> [Icon] -> [(String, String)]
validation validationPredicates icons =
  foldl
    (\acc predicate ->
       case predicate icons of
         Nothing -> acc
         Just (validationError, hint) -> (validationError, hint) : acc)
    []
    validationPredicates

process :: DrakonRendererArguments -> IO ()
process (DrakonRendererArguments inputPath layoutOutputPath balancedPathsOutputPath svgOutputPath) = do
  fileSizeInBytes <- getFileSize inputPath
  if fileSizeInBytes > maxInputFileSizeInBytes
    then putStrLn
           $ "Problem with diagram file \""
               <> inputPath
               <> "\" ("
               <> show fileSizeInBytes
               <> " bytes). Max allowed input file size: "
               <> show maxInputFileSizeInBytes
               <> " bytes."
    else do
      content <- catch (Data.ByteString.Lazy.readFile inputPath) handleReadError
      case decode content :: Maybe [Icon] of
        Just icons -> do
          let validationErrors =
                validation
                  [ oneTitleIconPresent
                  , oneEndIconPresent
                  , correctNumberOfDependencies
                  ]
                  icons
          case validationErrors of
            [] -> do
              titleIcon <-
                maybe
                  (fail
                     $ "No icons of type \""
                         <> show Title
                         <> "\" detected in the input.")
                  return
                  (findTitleIcon icons)
              endIcon <-
                maybe
                  (fail
                     $ "No icons of type \""
                         <> show End
                         <> "\" detected in the input.")
                  return
                  (findEndIcon icons)
              let paths = dcPaths [[titleIcon]] icons [endIcon]
              let bPaths = balance paths 1
              let printableBPaths =
                    showBalancedPathsHeader bPaths
                      <> "\n"
                      <> showBalancedPaths bPaths
              let prettyMarkdown =
                    pack $ "# balanced paths\n" <> printableBPaths
              bPathsOutputHandle <- openFile balancedPathsOutputPath WriteMode
              hPutStr bPathsOutputHandle prettyMarkdown
              hClose bPathsOutputHandle
              let refreshedPositionedIcons = positionIcons bPaths
              layoutOutputhandle <- openFile layoutOutputPath WriteMode
              hPutStr layoutOutputhandle (encodePretty refreshedPositionedIcons)
              hClose layoutOutputhandle
              let thisIsJustTemporary =
                    renderAllConnections refreshedPositionedIcons
              renderSVG' svgOutputPath svgOptions
                $ renderAllIcons refreshedPositionedIcons
                    <> snd thisIsJustTemporary
            _ -> do
              let failureReasons =
                    foldl
                      (\acc (validationError, hint) ->
                         acc
                           <> "* Error: "
                           <> validationError
                           <> " Hint: "
                           <> hint
                           <> "\n")
                      ""
                      validationErrors
              putStrLn
                $ "Input validation did not succeed for following reasons:\n"
                    <> failureReasons
        Nothing -> do
          let unpackedContent = unpack content
          putStrLn
            $ "Problem interpreting diagram file \""
                <> inputPath
                <> "\". Details: "
                <> unpackedContent
