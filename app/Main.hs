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
                    [skewerBlocks]
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
  let newDiagram@(DrakonDiagram _ _ _) =
        DrakonDiagram
          (Title (ID "100") (p2 (-1.0, -1.0)) (Content "bus journey"))
          [ [ Headline (ID "101") (p2 (-1.0, -1.0)) (Content "skewer 1 headline")
            , Action (ID "200") (p2 (-1.0, -1.0)) (Content "find a bus stop")
            , Fork
                (ID "300")
                (p2 (-1.0, -1.0))
                (Content "has a bus arrived?")
                (ConnectedSkewerBlocks [Action (ID "400") (p2 (-1.0, -1.0)) (Content "passengers boarding")] Nothing)
                (ConnectedSkewerBlocks [] (Just (ID "500")))
            , Fork
                (ID "500")
                (p2 (-1.0, -1.0))
                (Content "is it your turn?")
                (ConnectedSkewerBlocks [] Nothing)
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
            , Address (ID "1501") (p2 (-1.0, -1.0)) (Content "skewer 1 address")
            ]
          , [ Headline (ID "1599") (p2 (-1.0, -1.0)) (Content "skewer 2 headline")
            , Action (ID "1600") (p2 (-1.0, -1.0)) (Content "skewer 2 action")
            , Address (ID "1700") (p2 (-1.0, -1.0)) (Content "skewer 2 address")
            ]
          , [ Headline (ID "1599a") (p2 (-1.0, -1.0)) (Content "skewer 3 headline")
            , Action (ID "1600a") (p2 (-1.0, -1.0)) (Content "skewer 3 action")
            , Action (ID "1601a") (p2 (-1.0, -1.0)) (Content "skewer 3 action")
            , Address (ID "1700a") (p2 (-1.0, -1.0)) (Content "skewer 3 address")
            ]
          , [ Headline (ID "1599b") (p2 (-1.0, -1.0)) (Content "skewer 4 headline")
            , Action (ID "1600b") (p2 (-1.0, -1.0)) (Content "skewer 4 action")
            , Action (ID "1601b") (p2 (-1.0, -1.0)) (Content "skewer 4 action")
            , Action (ID "1602b") (p2 (-1.0, -1.0)) (Content "skewer 4 action")
            , Address (ID "1700b") (p2 (-1.0, -1.0)) (Content "skewer 4 address")
            ]
          ]
          (End (ID "1000000") (p2 (-1.0, -1.0)) (Content "end"))
  print $ Drakon.DrakonDiagram.heightInUnits newDiagram
  print $ Drakon.DrakonDiagram.widthInUnits newDiagram
  let addressY = (-1.0) * Drakon.DrakonDiagram.heightInUnits newDiagram + defaultBoundingBoxHeight * 2.0 -- ignore the heights of start and end terminators
  renderSVG' svgOutputPath svgOptions $ Drakon.DrakonDiagram.render newDiagram addressY
  -- let diagramInput =
  --       "Title [ Action Fork [ Action Action Action ] [ Action Action Fork [ Action ] [ Action Action ] ] Action ] End"
  -- case parse diagramInput of
  --   Left e -> putStrLn e
  --   Right diagram@(DrakonDiagram _ _ _ _) -> do
  --     renderSVG' svgOutputPath svgOptions $ render diagram
