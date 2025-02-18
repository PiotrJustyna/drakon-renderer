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

rect' :: Double -> Double -> Diagram B
rect' x y = fromOffsets [V2 x 0.0, V2 0.0 (y * (-1.0)), V2 (x * (-1.0)) 0.0, V2 0.0 y] # closeLine # strokeLoop

hex' :: Double -> Double -> Diagram B
hex' x y =
  fromOffsets
    [ V2 (x - 0.1 - 0.1) 0.0
    , V2 0.1 (y * (-0.5))
    , V2 (-0.1) (y * (-0.5))
    , V2 ((x - 0.1 - 0.1) * (-1.0)) 0.0
    , V2 (-0.1) (y * 0.5)
    , V2 0.1 (y * 0.5)
    ]
    # closeLine
    # strokeLoop

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

defaultBoundingBoxWidth :: Double
defaultBoundingBoxWidth = 3.0

defaultBoundingBoxHeight :: Double
defaultBoundingBoxHeight = 1.0

widthRatio :: Double
widthRatio = 0.8

-- colours used:
-- https://www.colourlovers.com/palette/541086/Loyal_Friends
lineColour :: Colour Double
lineColour = sRGB (160.0 / 255.0) (194.0 / 255.0) (222.0 / 255.0)

fillColour :: Colour Double
fillColour = sRGB (237.0 / 255.0) (237.0 / 255.0) (244.0 / 255.0)

fontColour :: Colour Double
fontColour = sRGB (6.0 / 255.0) (71.0 / 255.0) (128.0 / 255.0)

troubleshootingMode :: Bool
troubleshootingMode = False

renderedConnection :: [Point V2 Double] -> Diagram B
renderedConnection coordinates = fromVertices coordinates # lc lineColour # lw veryThin

defaultFontSize :: Double
defaultFontSize = defaultBoundingBoxHeight / 6.0

renderText :: String -> Double -> Double -> Diagram B
renderText content translateX translateY =
  text content
    # fontSize (local defaultFontSize)
    # light
    # font "helvetica"
    # fc fontColour
    # translate (r2 (translateX, translateY))

class Renderer a where
  render :: a -> Point V2 Double -> Diagram B
  widthInUnits :: a -> Double
  heightInUnits :: a -> Double

data DrakonDiagram =
  DrakonDiagram StartTerminator [SkewerBlock] EndTerminator

data StartTerminator
  = Title String
  | CyclicStart String
  | TitleWithParameters String
  | CyclicStartWithParameters String

newtype EndTerminator =
  End String

data ValentPoint =
  ValentPoint

data SkewerBlock
  = Action String
  | Question String
  | Fork String [SkewerBlock] [SkewerBlock]
  -- | ArrowLoop [SkewerBlock] [SkewerBlock]

instance Show DrakonDiagram where
  show diagram =
    "diagram total width in units: "
      <> show (widthInUnits diagram)
      <> "\n"
      <> "diagram total height in units: "
      <> show (heightInUnits diagram)

instance Renderer StartTerminator where
  render title@(Title content) origin =
    position
      [ ( origin
        , renderText
            content
            (0.0 + widthInUnits title * defaultBoundingBoxWidth * 0.5)
            (0.0 - heightInUnits title * defaultBoundingBoxHeight * 0.5)
            <> (roundedRect
                  (widthInUnits title * defaultBoundingBoxWidth * widthRatio)
                  (heightInUnits title * defaultBoundingBoxHeight * 0.5)
                  0.5
                  # lw veryThin
                  # lc lineColour
                  # fc fillColour
                  # translate (r2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-0.5))))
            <> if troubleshootingMode
                 then (rect'
                         (widthInUnits title * defaultBoundingBoxWidth)
                         (heightInUnits title * defaultBoundingBoxHeight)
                         # lw veryThin
                         # lc lineColour)
                 else mempty)
      ]
  render _ _ = mempty
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

instance Renderer EndTerminator where
  render (End content) = render (Title content)
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

instance Renderer ValentPoint where
  render ValentPoint origin =
    position
      [ ( origin
        , if troubleshootingMode
            then rect'
                   (widthInUnits ValentPoint * defaultBoundingBoxWidth)
                   (heightInUnits ValentPoint * defaultBoundingBoxHeight)
                   # lw veryThin
                   # lc lineColour
            else mempty)
      ]
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

render' :: [SkewerBlock] -> Point V2 Double -> (Diagram B, Double)
render' skewerBlocks (P (V2 x y)) =
  foldl
    (\accu singleBlock ->
       let diagram = fst accu
           preY1 = snd accu
           connectionX = x + defaultBoundingBoxWidth * 0.5
           preY2 = preY1 - defaultBoundingBoxHeight * 0.25
           postY1 = preY2 - defaultBoundingBoxHeight * 0.5
           postY2 = preY1 - defaultBoundingBoxHeight
        in ( renderedConnection [p2 (connectionX, preY1), p2 (connectionX, preY2)]
               <> diagram
               <> renderedConnection [p2 (connectionX, postY1), p2 (connectionX, postY2)]
               <> render singleBlock (P (V2 x (snd accu)))
           , snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
    (mempty, y)
    skewerBlocks

widthInUnits' :: [SkewerBlock] -> Double
widthInUnits' skewerBlocks = maximum $ map widthInUnits skewerBlocks

heightInUnits' :: [SkewerBlock] -> Double
heightInUnits' skewerBlocks = sum $ map heightInUnits skewerBlocks

instance Renderer SkewerBlock where
  render action@(Action content) origin =
    let iconHeight = heightInUnits action * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                content
                (0.0 + widthInUnits action * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits action * defaultBoundingBoxHeight * 0.5)
                <> rect' (widthInUnits action * defaultBoundingBoxWidth * widthRatio) iconHeight
                     # lw veryThin
                     # lc lineColour
                     # fc fillColour
                     # translate (r2 (defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                     then rect'
                            (widthInUnits action * defaultBoundingBoxWidth)
                            (heightInUnits action * defaultBoundingBoxHeight)
                            # lw veryThin
                            # lc lineColour
                     else mempty)
          ]
  render question@(Question content) origin =
    let iconHeight = heightInUnits question * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , renderText
                content
                (0.0 + widthInUnits question * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits question * defaultBoundingBoxHeight * 0.5)
                <> hex' (widthInUnits question * defaultBoundingBoxWidth * widthRatio) iconHeight
                     # lw veryThin
                     # lc lineColour
                     # fc fillColour
                     # translate (r2 (0.1 + defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                     then rect'
                            (widthInUnits question * defaultBoundingBoxWidth)
                            (heightInUnits question * defaultBoundingBoxHeight)
                            # lw veryThin
                            # lc lineColour
                     else mempty)
          ]
  render fork@(Fork content l r) origin@(P (V2 x y)) =
    let question = Question content
        lOrigin = P (V2 x (y - heightInUnits question * defaultBoundingBoxHeight))
        rOrigin@(P (V2 rX rY)) =
          P (V2 (x + widthInUnits' l * defaultBoundingBoxWidth) (y - heightInUnits question * defaultBoundingBoxHeight))
        connectionLX = x + defaultBoundingBoxWidth * 0.5
     in render question origin
          <> if null l
               then render ValentPoint lOrigin
               else renderText
                      "no"
                      (x + widthInUnits question * defaultBoundingBoxWidth * 0.97)
                      (y - heightInUnits question * defaultBoundingBoxHeight * 0.35)
                      <> renderText
                           "yes"
                           (x + widthInUnits question * defaultBoundingBoxWidth * 0.42)
                           (y - heightInUnits question * defaultBoundingBoxHeight * 0.9)
                      <> fst (render' l lOrigin)
                      <> renderedConnection
                           [ p2 (connectionLX, y - heightInUnits' l * defaultBoundingBoxHeight)
                           , p2 (connectionLX, y - heightInUnits fork * defaultBoundingBoxHeight)
                           ]
                      <> renderedConnection
                           [ p2
                               ( x + widthInUnits question * defaultBoundingBoxWidth * (widthRatio + 1) / 2.0
                               , y - heightInUnits question * defaultBoundingBoxHeight * 0.5)
                           , p2
                               ( rX + defaultBoundingBoxWidth * 0.5
                               , y - heightInUnits question * defaultBoundingBoxHeight * 0.5)
                           , p2 (rX + defaultBoundingBoxWidth * 0.5, rY - defaultBoundingBoxHeight * 0.25)
                           ]
                      <> if null r
                           then render ValentPoint rOrigin
                                  <> renderedConnection
                                       [ p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits ValentPoint * defaultBoundingBoxHeight)
                                       , p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       , p2
                                           ( x + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       ]
                           else fst (render' r rOrigin)
                                  <> position
                                       [ ( origin
                                         , if troubleshootingMode
                                             then rect'
                                                    (widthInUnits fork * defaultBoundingBoxWidth)
                                                    (heightInUnits fork * defaultBoundingBoxHeight)
                                                    # lw veryThin
                                                    # lc lineColour
                                             else mempty)
                                       ]
                                  <> renderedConnection
                                       [ p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits' r * defaultBoundingBoxHeight)
                                       , p2
                                           ( rX + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       , p2
                                           ( x + defaultBoundingBoxWidth * 0.5
                                           , y - heightInUnits fork * defaultBoundingBoxHeight)
                                       ]
  widthInUnits (Action _) = 1.0
  widthInUnits (Question _) = 1.0
  widthInUnits (Fork _ l r) =
    (if null l
       then widthInUnits ValentPoint
       else widthInUnits' l)
      + (if null r
           then widthInUnits ValentPoint
           else widthInUnits' r)
  heightInUnits (Action _) = 1.0
  heightInUnits (Question _) = 1.0
  heightInUnits (Fork content l r) =
    let question = Question content
     in heightInUnits question
          + max
              (if null l
                 then heightInUnits ValentPoint
                 else heightInUnits' l)
              (if null r
                 then heightInUnits ValentPoint
                 else heightInUnits' r)

instance Renderer DrakonDiagram where
  render (DrakonDiagram startTerminator skewerBlocks endTerminator) origin@(P (V2 x y)) =
    let connectionX = x + widthInUnits startTerminator * defaultBoundingBoxWidth * 0.5
        skewerY = heightInUnits startTerminator * defaultBoundingBoxHeight
        startY1 = y - skewerY * 0.75
        startY2 = y - defaultBoundingBoxHeight
        renderedSkewerBlocks = render' skewerBlocks (p2 (x, y - skewerY))
        finishY1 = snd renderedSkewerBlocks
        finishY2 = finishY1 - defaultBoundingBoxHeight * 0.25
     in render startTerminator origin
          <> renderedConnection [p2 (connectionX, startY1), p2 (connectionX, startY2)]
          <> fst renderedSkewerBlocks
          <> renderedConnection [p2 (connectionX, finishY1), p2 (connectionX, finishY2)]
          <> render endTerminator (P (V2 x (snd renderedSkewerBlocks)))
  widthInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator) =
    maximum $ widthInUnits startTerminator : map widthInUnits skewerBlocks ++ [widthInUnits endTerminator]
  heightInUnits (DrakonDiagram startTerminator skewerBlocks endTerminator) =
    sum $ heightInUnits startTerminator : map heightInUnits skewerBlocks ++ [heightInUnits endTerminator]

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
    "Action" -> Right (Just (Action "custom content - action"), ts)
    "Fork" ->
      case parseSkewerBlocks ts of
        Left e -> Left e
        Right (lSkewerBlocks, ts') ->
          case parseSkewerBlocks ts' of
            Left e -> Left e
            Right (rSkewerBlocks, ts'') -> Right (Just (Fork "custom content - fork" lSkewerBlocks rSkewerBlocks), ts'')
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
  let newTypesDiagram =
        "Title [ Action Fork [ Action Action Action ] [ Action Action Fork [ Action ] [ Action Action ] ] Action ] End"
  let diagram3 = "Title [ Fork [ Action Action ] [ ] ] End"
  let diagram4 = "Title [ Fork [ Action Action Action Action ] [ Fork [ Action ] [ Action ] ] ] End"
  case parse newTypesDiagram of
    Left e -> putStrLn e
    Right diagram -> do
      print diagram
      renderSVG' svgOutputPath svgOptions $ render diagram (p2 (0.0, 0.0))
