module Main where

import Diagrams.Backend.SVG (renderSVG')
import Diagrams.Prelude (p2)
import Drakon.Constants (svgOptions, svgOutputPath)
import Drakon.Content (Content(Content))
import Drakon.DrakonDiagram (DrakonDiagram(..))
import Drakon.EndTerminator (EndTerminator(End))
import Drakon.ID (ID(ID))
import Drakon.SkewerBlock (SkewerBlock(Action, Fork), ConnectedSkewerBlocks(ConnectedSkewerBlocks))
import Drakon.StartTerminator (StartTerminator(Title))
import Drakon.TypeClasses (render)

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
              Right
                ( DrakonDiagram
                    (Title (ID "-1") (p2 (-1.0, -1.0)) (Content "custom content - title"))
                    skewerBlocks
                    endTerminator
                    [(ID "4.0", ID "-5.0")]
                , [])
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
    "Action" -> Right (Just (Action (ID "-1") (p2 (-1.0, -1.0)) (Content "custom content - action")), ts)
    "Fork" ->
      case parseSkewerBlocks ts of
        Left e -> Left e
        Right (lSkewerBlocks, ts') ->
          case parseSkewerBlocks ts' of
            Left e -> Left e
            Right (rSkewerBlocks, ts'') ->
              Right
                (Just (Fork (ID "-1") (p2 (-1.0, -1.0)) (Content "custom content - fork") (ConnectedSkewerBlocks lSkewerBlocks Nothing) (ConnectedSkewerBlocks rSkewerBlocks Nothing))
                , ts'')
    "]" -> Right (Nothing, ts)
    _ -> Left $ "unexpected token: " <> t

parseEndTerminator :: [String] -> Either String (EndTerminator, [String])
parseEndTerminator [] = Left "no finish terminator tokens provided"
parseEndTerminator (t:ts) =
  case t of
    "End" -> Right (End (ID "-1") (p2 (-1.0, -1.0)) (Content "custom content - end"), ts)
    _ -> Left $ "unexpected token: " <> t

main :: IO ()
main = do
  let newDiagram@(DrakonDiagram _ _ _ _) =
        DrakonDiagram
          (Title (ID "100") (p2 (-1.0, -1.0)) (Content "custom content - title"))
          [ Action (ID "200") (p2 (-1.0, -1.0)) (Content "custom content - action")
          , Action (ID "201") (p2 (-1.0, -1.0)) (Content "custom content - action")
          , Fork
              (ID "210")
              (p2 (-1.0, -1.0))
              (Content "custom content - fork test")
              (ConnectedSkewerBlocks [Action (ID "220") (p2 (-1.0, -1.0)) (Content "custom content - action")] Nothing)
              (ConnectedSkewerBlocks [Action (ID "221") (p2 (-1.0, -1.0)) (Content "custom content - action")] Nothing)
          ]
          (End (ID "300") (p2 (-1.0, -1.0)) (Content "custom content - end"))
          -- []
          [(ID "221", ID "200"), (ID "221", ID "201")]
  -- let newFork =
  --       NewFork
  --         (ID "210")
  --         (Content "custom content - fork")
  --         (ConnectedSkewerBlocks [NewAction (ID "220") (Content "custom content - action")] Nothing)
  --         (ConnectedSkewerBlocks [] (Just (ID "200")))
  -- print newFork
  renderSVG' svgOutputPath svgOptions $ render newDiagram
  -- let diagramInput =
  --       "Title [ Action Fork [ Action Action Action ] [ Action Action Fork [ Action ] [ Action Action ] ] Action ] End"
  -- case parse diagramInput of
  --   Left e -> putStrLn e
  --   Right diagram@(DrakonDiagram _ _ _ _) -> do
  --     renderSVG' svgOutputPath svgOptions $ render diagram
