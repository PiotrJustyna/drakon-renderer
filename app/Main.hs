module Main where

import Diagrams.Backend.SVG (renderSVG')
import Diagrams.Prelude (p2)
import Drakon.Constants (defaultBoundingBoxHeight, svgOptions, svgOutputPath)
import Drakon.Content (Content(Content))
import Drakon.DrakonDiagram (DrakonDiagram(..), heightInUnits, render)
import Drakon.EndTerminator (EndTerminator(End))
import Drakon.ID (ID(ID))
import Drakon.SkewerBlock(reverse'')
import Drakon.StartTerminator (StartTerminator(Title))
import Lexer (alexScanTokens)
import Parser (ParseResult(..), diagram)

main :: IO ()
main = do
  fileContent <- readFile "./app/Drakon/input.txt"
  putStrLn "tokens:"
  let tokens = alexScanTokens fileContent
  print tokens
  case diagram tokens 1 of
    ParseOk d -> do
      print $ length d
      let blocks = reverse'' d
          drakonDiagram =
            DrakonDiagram
              (Title (ID "-1") (p2 (-1.0, -1.0)) (Content "start"))
              blocks
              (End (ID "-1") (p2 (-1.0, -1.0)) (Content "end"))
          addressY = (-1.0) * Drakon.DrakonDiagram.heightInUnits drakonDiagram + defaultBoundingBoxHeight * 2.0 -- ignore the heights of start and end terminators
      renderSVG' svgOutputPath svgOptions $ Drakon.DrakonDiagram.render drakonDiagram addressY
    ParseFail s -> error s
