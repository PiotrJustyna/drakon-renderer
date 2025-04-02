module Main where

import Data.Map (empty)
import Diagrams.Backend.SVG (renderSVG')
import Diagrams.Prelude (Point(..), V2(..), p2)
import Drakon.Constants (svgOptions, svgOutputPath)
import Drakon.Content (Content(Content))
import Drakon.DrakonDiagram (DrakonDiagram(..))
import Drakon.EndTerminator (EndTerminator(End))
import Drakon.ID (ID(ID))
import Drakon.SkewerBlock
  ( ConnectedSkewerBlocks(ConnectedSkewerBlocks)
  , SkewerBlock(Action, Fork)
  , insertToMap
  , position'
  , toMap
  )
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
                ( Just
                    (Fork
                       (ID "-1")
                       (p2 (-1.0, -1.0))
                       (Content "custom content - fork")
                       (ConnectedSkewerBlocks lSkewerBlocks Nothing)
                       (ConnectedSkewerBlocks rSkewerBlocks Nothing))
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
-- just a test
-- DrakonDiagram
--   (Title (ID "100") (p2 (-1.0, -1.0)) (Content "custom content - title"))
--   [ Action (ID "200") (p2 (-1.0, -1.0)) (Content "custom content - action")
--   , Action (ID "201") (p2 (-1.0, -1.0)) (Content "custom content - action")
--   , Fork
--       (ID "210")
--       (p2 (-1.0, -1.0))
--       (Content "custom content - fork test 1")
--       (ConnectedSkewerBlocks
--          [Action (ID "220") (p2 (-1.0, -1.0)) (Content "custom content - action")]
--          Nothing)
--       (ConnectedSkewerBlocks
--          [Action (ID "221") (p2 (-1.0, -1.0)) (Content "custom content - action 221"),
--           Action (ID "222") (p2 (-1.0, -1.0)) (Content "custom content - action 222"),
--           Fork
--               (ID "223")
--               (p2 (-1.0, -1.0))
--               (Content "custom content - fork test 2")
--               (ConnectedSkewerBlocks
--                  [Action (ID "224") (p2 (-1.0, -1.0)) (Content "custom content - action")]
--                  (Just (ID "200")))
--               (ConnectedSkewerBlocks
--                  [Action (ID "225") (p2 (-1.0, -1.0)) (Content "custom content - action 225"),
--                   Action (ID "226") (p2 (-1.0, -1.0)) (Content "custom content - action 226")]
--                  (Just (ID "200")))]
--          Nothing)
--   ]
--   (End (ID "300") (p2 (-1.0, -1.0)) (Content "custom content - end"))
-- Fig.6:
  let newDiagram@(DrakonDiagram _ _ _) =
        DrakonDiagram
          (Title (ID "100") (p2 (-1.0, -1.0)) (Content "bus journey"))
          [ Action (ID "200") (p2 (-1.0, -1.0)) (Content "find a bus stop")
          , Fork
              (ID "300")
              (p2 (-1.0, -1.0))
              (Content "has a bus arrived?")
              (ConnectedSkewerBlocks [Action (ID "400") (p2 (-1.0, -1.0)) (Content "passengers boarding")] Nothing)
              (ConnectedSkewerBlocks
                 []
                 (Just (ID "410")))
          , Fork
              (ID "500")
              (p2 (-1.0, -1.0))
              (Content "is it your turn?")
              (ConnectedSkewerBlocks
                 []
                 Nothing)
              (ConnectedSkewerBlocks
                 [Action (ID "610") (p2 (-1.0, -1.0)) (Content "wait for your turn")]
                 (Just (ID "500")))
          , Fork
              (ID "700")
              (p2 (-1.0, -1.0))
              (Content "is it possible to enter the bus?")
              (ConnectedSkewerBlocks [Action (ID "800") (p2 (-1.0, -1.0)) (Content "enter the bus")] Nothing)
              (ConnectedSkewerBlocks [] (Just (ID "410")))
          , Fork
              (ID "900")
              (p2 (-1.0, -1.0))
              (Content "are any seats available?")
              (ConnectedSkewerBlocks [Action (ID "1000") (p2 (-1.0, -1.0)) (Content "take a seat")] Nothing)
              (ConnectedSkewerBlocks
                 [ Fork
                     (ID "1010")
                     (p2 (-1.0, -1.0))
                     (Content "do you want to travel standing?")
                     (ConnectedSkewerBlocks [] Nothing)
                     (ConnectedSkewerBlocks
                        [ Action (ID "1010") (p2 (-1.0, -1.0)) (Content "leave the bus")
                        , Action (ID "410") (p2 (-1.0, -1.0)) (Content "wait for the next bus")
                        ]
                        (Just (ID "300")))
                 ]
                 Nothing)
          , Fork
              (ID "1200")
              (p2 (-1.0, -1.0))
              (Content "do you have money for a ticket?")
              (ConnectedSkewerBlocks [Action (ID "1300") (p2 (-1.0, -1.0)) (Content "buy a ticket")] Nothing)
              (ConnectedSkewerBlocks [] Nothing)
          , Action (ID "1400") (p2 (-1.0, -1.0)) (Content "travel to the required stop")
          , Action (ID "1500") (p2 (-1.0, -1.0)) (Content "leave the bus")
          ]
          (End (ID "1000000") (p2 (-1.0, -1.0)) (Content "end"))
  renderSVG' svgOutputPath svgOptions $ render newDiagram empty
  -- let diagramInput =
  --       "Title [ Action Fork [ Action Action Action ] [ Action Action Fork [ Action ] [ Action Action ] ] Action ] End"
  -- case parse diagramInput of
  --   Left e -> putStrLn e
  --   Right diagram@(DrakonDiagram _ _ _ _) -> do
  --     renderSVG' svgOutputPath svgOptions $ render diagram
