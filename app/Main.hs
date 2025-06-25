module Main where

import Diagrams.Backend.SVG (renderSVG')
import Diagrams.Prelude (p2)
import Drakon.Constants (defaultBoundingBoxHeight, svgOptions, svgOutputPath)
import Drakon.Content (Content(Content))
import Drakon.DrakonDiagram (DrakonDiagram(..), heightInUnits, render, widthInUnits)
import Drakon.EndTerminator (EndTerminator(End))
import Drakon.ID (ID(ID))
import Drakon.SkewerBlock (ConnectedSkewerBlocks(ConnectedSkewerBlocks), SkewerBlock(Action, Address, Fork, Headline))
import Drakon.StartTerminator (StartTerminator(Title))
import Lexer (alexScanTokens)

-- 2025-06-19 PJ:
-- --------------
-- For now the assumption is that we are only
-- dealing with a 1D list of skewer blocks.
-- No forks, no silhouette diagrams -
-- that will come later.
parse :: [(String, String)] -> Either String (DrakonDiagram, [String])
parse [] = Left "no tokens provided"
parse (t:ts) =
  case t of
    ("block", x) ->
      case parseSkewerBlocks' (init ts) of
        Left e -> Left e
        Right (skewerBlocks, _) ->
          case parseEndTerminator (last ts) of
            Left e -> Left e
            Right endTerminator ->
              Right ((DrakonDiagram (Title (ID "1") (p2 (-1.0, -1.0)) (Content x))
                    [skewerBlocks]
                    endTerminator), [])
    (x, _) -> Left $ "unexpected token: " <> x

parseSkewerBlocks' :: [(String, String)] -> Either String ([SkewerBlock], [(String, String)])
parseSkewerBlocks' [] = Left "no tokens provided"
parseSkewerBlocks' tokens = combine . loop $ parseSkewerBlock' tokens

loop :: Either String (Maybe SkewerBlock, [(String, String)]) -> [Either String (Maybe SkewerBlock, [(String, String)])]
loop (Left e) = [Left e]
loop x@(Right (Nothing, _)) = [x]
loop x@(Right (Just _, ts)) = x : loop (parseSkewerBlock' ts)

combine :: [Either String (Maybe SkewerBlock, [(String, String)])] -> Either String ([SkewerBlock], [(String, String)])
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

-- 2025-06-17 PJ:
-- --------------
-- We can return "Left" later, if we cannot parse.
parseSkewerBlock' :: [(String, String)] -> Either String (Maybe SkewerBlock, [(String, String)])
parseSkewerBlock' [] = Right (Nothing, [])
parseSkewerBlock' (t:ts) = Right (Just (Action (ID "-1") (p2 (-1.0, -1.0)) (Content (snd t))), ts)

parseEndTerminator :: (String, String) -> Either String EndTerminator
parseEndTerminator (x, y) = Right (End (ID "-1") (p2 (-1.0, -1.0)) (Content "custom content - end"))

main :: IO ()
main = do
  fileContent <- readFile "./app/Drakon/input.txt"
  let possiblyDiagram = parse $ alexScanTokens fileContent

  case possiblyDiagram of
    Left e -> print e
    Right (diagram, _) -> do
      let addressY = (-1.0) * Drakon.DrakonDiagram.heightInUnits diagram + defaultBoundingBoxHeight * 2.0 -- ignore the heights of start and end terminators
      renderSVG' svgOutputPath svgOptions $ Drakon.DrakonDiagram.render diagram addressY
