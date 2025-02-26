module Main where

import Data.Colour.SRGB (sRGB)
import Data.Text (empty)
import Diagrams.Backend.SVG
  ( B
  , Options(SVGOptions)
  , SVG
  , _generateDoctype
  , _idPrefix
  , _size
  , _svgAttributes
  , _svgDefinitions
  , renderSVG'
  )
import Diagrams.Prelude
  ( Colour
  , Diagram
  , Point(..)
  , V2(..)
  , (#)
  , closeLine
  , fc
  , font
  , fontSize
  , fromOffsets
  , fromVertices
  , lc
  , light
  , local
  , lw
  , mkSizeSpec
  , p2
  , position
  , r2
  , roundedRect
  , strokeLoop
  , text
  , translate
  , veryThin
  )
import Drakon.Content (Content(Content))
import Drakon.DrakonDiagram (DrakonDiagram(..))
import Drakon.EndTerminator (EndTerminator(End))
import Drakon.ID (ID(ID))
import Drakon.SkewerBlock (SkewerBlock(Action, Fork))
import Drakon.StartTerminator (StartTerminator(Title))
import Drakon.TypeClasses (Renderer, render)

svgOptions :: Num n => Options SVG V2 n
svgOptions =
  SVGOptions
    { _size = mkSizeSpec $ V2 (Just 1000) (Just 1000)
    , _idPrefix = empty
    , _svgDefinitions = Nothing
    , _svgAttributes = []
    , _generateDoctype = True
    }

svgOutputPath :: String
svgOutputPath = "./new-types-diagram.svg"

parse :: String -> Either String DrakonDiagram
parse x =
  case parse' (words x) of
    Left e -> Left e
    Right (diagram, []) -> Right diagram
    Right (_, moreTokens) -> Left $ "unexpected tokens: " <> unwords moreTokens

parse' :: [String] -> Either String (DrakonDiagram, [String])
parse' [] = Left "unexpected end of expression"
parse' (t:ts) =
  case t of
    "Title" ->
      case parseSkewerBlocks ts of
        Left e -> Left e
        Right (skewerBlocks, ts') ->
          case parseEndTerminator ts' of
            Left e -> Left e
            Right (endTerminator, _) ->
              Right (DrakonDiagram (Title "custom content - title") skewerBlocks endTerminator, [])
    _ -> Left $ "unexpected token: " <> t

parseSkewerBlocks :: [String] -> Either String ([SkewerBlock], [String])
parseSkewerBlocks [] = Left "no skewer block tokens provided"
parseSkewerBlocks (t:ts) =
  case t of
    "[" -> combine . loop $ parseSkewerBlock ts
    _ -> Left $ "unexpected token: " <> t

loop :: Either String (Maybe SkewerBlock, [String]) -> [Either String (Maybe SkewerBlock, [String])]
loop (Left e) = [Left e]
loop x@(Right (Nothing, _)) = [x]
loop x@(Right (Just _, ts)) = x : loop (parseSkewerBlock ts)

combine :: [Either String (Maybe SkewerBlock, [String])] -> Either String ([SkewerBlock], [String])
combine =
  foldl
    (\accu x ->
       case accu of
         Left e -> Left e
         Right (skewerBlocks, _) ->
           case x of
             Left e -> Left e
             Right (Nothing, ts') -> Right (skewerBlocks, ts')
             Right (Just skewerBlock, ts') -> Right (skewerBlocks ++ [skewerBlock], ts'))
    (Right ([], []))

parseSkewerBlock :: [String] -> Either String (Maybe SkewerBlock, [String])
parseSkewerBlock [] = Left "no skewer block tokens provided"
parseSkewerBlock (t:ts) =
  case t of
    "Action" -> Right (Just (Action (ID "-1") (Content "custom content - action")), ts)
    "Fork" ->
      case parseSkewerBlocks ts of
        Left e -> Left e
        Right (lSkewerBlocks, ts') ->
          case parseSkewerBlocks ts' of
            Left e -> Left e
            Right (rSkewerBlocks, ts'') ->
              Right (Just (Fork (ID "-1") (Content "custom content - fork") lSkewerBlocks rSkewerBlocks), ts'')
    "]" -> Right (Nothing, ts)
    _ -> Left $ "unexpected token: " <> t

parseEndTerminator :: [String] -> Either String (EndTerminator, [String])
parseEndTerminator [] = Left "no finish terminator tokens provided"
parseEndTerminator (t:ts) =
  case t of
    "End" -> Right (End "custom content - end", ts)
    _ -> Left $ "unexpected token: " <> t

main :: IO ()
main = do
  let diagramInput =
        "Title [ Action Fork [ Action Action Action ] [ Action Action Fork [ Action ] [ Action Action ] ] Action ] End"
  case parse diagramInput of
    Left e -> putStrLn e
    Right diagram -> do
      print diagram
      renderSVG' svgOutputPath svgOptions $ render diagram (p2 (0.0, 0.0))
