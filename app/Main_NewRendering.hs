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
  , fromOffsets
  , lc
  , lw
  , mkSizeSpec
  , p2
  , position
  , r2
  , roundedRect
  , strokeLoop
  , thick
  , translate
  , veryThin
  )

rect' :: Double -> Double -> Diagram B
rect' x y =
  fromOffsets [V2 x 0.0, V2 0.0 (y * (-1.0)), V2 (x * (-1.0)) 0.0, V2 0.0 y]

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
defaultBoundingBoxWidth = 2.0

defaultBoundingBoxHeight :: Double
defaultBoundingBoxHeight = 1.0

widthRatio :: Double
widthRatio = 0.8

lineColour :: Colour Double
lineColour = sRGB (34.0 / 255.0) (69.0 / 255.0) (57.0 / 255.0)

class Renderer a where
  render :: a -> Point V2 Double -> Diagram B
  widthInUnits :: a -> Double
  heightInUnits :: a -> Double

data DrakonDiagram =
  DrakonDiagram StartTerminator [SkewerBlock] FinishTerminator

data StartTerminator
  = Title
  | CyclicStart
  | TitleWithParameters
  | CyclicStartWithParameters

data FinishTerminator =
  End

data ValentPoint =
  ValentPoint

data Branch
  = EmptyBranch ValentPoint
  | FullBranch [SkewerBlock]

data SkewerBlock
  = Action
  | Question
  | Fork Branch Branch

instance Show DrakonDiagram where
  show diagram =
    "diagram total width in units: "
      <> show (widthInUnits diagram)
      <> "\n"
      <> "diagram total height in units: "
      <> show (heightInUnits diagram)

instance Renderer StartTerminator where
  render Title origin =
    position
      [ ( origin
        , (roundedRect
             (widthInUnits Title * defaultBoundingBoxWidth * widthRatio)
             (heightInUnits Title * defaultBoundingBoxHeight * 0.5)
             0.5
             # lw thick
             # lc lineColour
             # translate
                 (r2
                    ( defaultBoundingBoxWidth * 0.5
                    , defaultBoundingBoxHeight * (-0.5))))
            <> (rect'
                  (widthInUnits Title * defaultBoundingBoxWidth)
                  (heightInUnits Title * defaultBoundingBoxHeight)
                  # lw veryThin))
      ]
  render _ _ = mempty
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

instance Renderer FinishTerminator where
  render _ = render Title
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

instance Renderer ValentPoint where
  render ValentPoint origin =
    position
      [ ( origin
        , rect'
            (widthInUnits Action * defaultBoundingBoxWidth)
            (heightInUnits Action * defaultBoundingBoxHeight)
            # lw veryThin)
      ]
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

instance Renderer SkewerBlock where
  render Action origin =
    let iconHeight = heightInUnits Action * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , rect'
                (widthInUnits Action * defaultBoundingBoxWidth * widthRatio)
                iconHeight
                # lw thick
                # lc lineColour
                # translate
                    (r2
                       ( defaultBoundingBoxWidth * (1 - widthRatio) / 2.0
                       , iconHeight * (-0.5)))
                <> (rect'
                      (widthInUnits Action * defaultBoundingBoxWidth)
                      (heightInUnits Action * defaultBoundingBoxHeight)
                      # lw veryThin))
          ]
  render Question origin =
    let iconHeight = heightInUnits Action * defaultBoundingBoxHeight * 0.5
     in position
          [ ( origin
            , hex'
                (widthInUnits Action * defaultBoundingBoxWidth * widthRatio)
                iconHeight
                # lw thick
                # lc lineColour
                # translate
                    (r2
                       ( 0.1 + defaultBoundingBoxWidth * (1 - widthRatio) / 2.0
                       , iconHeight * (-0.5)))
                <> (rect'
                      (widthInUnits Action * defaultBoundingBoxWidth)
                      (heightInUnits Action * defaultBoundingBoxHeight)
                      # lw veryThin))
          ]
  render fork@(Fork l r) origin@(P (V2 x y)) =
    render Question origin
      <> render
           l
           (P (V2 x (y - heightInUnits Question * defaultBoundingBoxHeight)))
      <> render
           r
           (P (V2
                 (x + widthInUnits l * defaultBoundingBoxWidth)
                 (y - heightInUnits Question * defaultBoundingBoxHeight)))
      <> position
           [ ( origin
             , rect'
                 (widthInUnits fork * defaultBoundingBoxWidth)
                 (heightInUnits fork * defaultBoundingBoxHeight)
                 # lw veryThin)
           ]
  widthInUnits Action = 1.0
  widthInUnits Question = 1.0
  widthInUnits (Fork l r) = widthInUnits l + widthInUnits r
  heightInUnits Action = 1.0
  heightInUnits Question = 1.0
  heightInUnits (Fork l r) =
    heightInUnits Question + max (heightInUnits l) (heightInUnits r)

instance Renderer Branch where
  render (EmptyBranch _) _origin = render ValentPoint _origin
  render (FullBranch skewerBlocks) (P (V2 x y)) =
    fst
      $ foldl
          (\accu singleBlock ->
             ( fst accu <> render singleBlock (P (V2 x (snd accu)))
             , snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
          (mempty, y)
          skewerBlocks
  widthInUnits (EmptyBranch _) = 1.0
  widthInUnits (FullBranch skewerBlocks) =
    maximum $ map widthInUnits skewerBlocks
  heightInUnits (EmptyBranch _) = 1.0
  heightInUnits (FullBranch skewerBlocks) = sum $ map heightInUnits skewerBlocks

instance Renderer DrakonDiagram where
  render (DrakonDiagram startTerminator skewerBlocks finishTerminator) origin@(P (V2 x y)) =
    let renderedSkewerBlocks =
          foldl
            (\accu singleBlock ->
               ( fst accu <> render singleBlock (P (V2 x (snd accu)))
               , snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight))
            ( mempty
            , y - heightInUnits startTerminator * defaultBoundingBoxHeight)
            skewerBlocks
     in render startTerminator origin
          <> fst renderedSkewerBlocks
          <> render finishTerminator (P (V2 x (snd renderedSkewerBlocks)))
  widthInUnits (DrakonDiagram startTerminator skewerBlocks finishTerminator) =
    maximum
      $ widthInUnits startTerminator
          : map widthInUnits skewerBlocks
          ++ [widthInUnits finishTerminator]
  heightInUnits (DrakonDiagram startTerminator skewerBlocks finishTerminator) =
    sum
      $ heightInUnits startTerminator
          : map heightInUnits skewerBlocks
          ++ [heightInUnits finishTerminator]

parse :: String -> Either String DrakonDiagram
parse x =
  case parse' (words x) of
    Left error -> Left error
    Right (diagram, []) -> Right diagram
    Right (diagram, moreTokens) ->
      Left $ "unexpected tokens: " <> unwords moreTokens

parse' :: [String] -> Either String (DrakonDiagram, [String])
parse' [] = Left "unexpected end of expression"
parse' (t:ts) =
  case t of
    "Title" ->
      case parseSkewerBlocks ts of
        Left error -> Left error
        Right (skewerBlocks, ts') ->
          case parseFinishTerminator ts' of
            Left e -> Left e
            Right (finishTerminator, ts'') ->
              Right (DrakonDiagram Title skewerBlocks finishTerminator, [])
    _ -> Left $ "unexpected token 1: " <> t

parseSkewerBlocks :: [String] -> Either String ([SkewerBlock], [String])
parseSkewerBlocks (t:ts) = case t of
  "[" -> combine . loop $ parseSkewerBlock ts
  _ -> Left $ "unexpected token 2: " <> t

loop :: Either String (Maybe SkewerBlock, [String]) -> [Either String (Maybe SkewerBlock, [String])]
loop (Left e) = [Left e]
loop x@(Right (Nothing, ts)) = [x]
loop x@(Right (Just _, ts)) = x : loop (parseSkewerBlock ts)

combine :: [Either String (Maybe SkewerBlock, [String])] -> Either String ([SkewerBlock], [String])
combine = foldl
  (\accu x -> case accu of
    Left e -> Left e
    Right (skewerBlocks, _) ->
      case x of
        Left e -> Left e
        Right (Nothing, ts') -> Right (skewerBlocks, ts')
        Right (Just skewerBlock, ts') -> Right (skewerBlock : skewerBlocks, ts'))
  (Right ([], []))

parseSkewerBlock :: [String] -> Either String (Maybe SkewerBlock, [String])
parseSkewerBlock (t:ts) =
  case t of
    "Action" -> Right (Just Action, ts)
    "Question" -> Right (Just Question, ts)
    "]" -> Right (Nothing, ts)
    _ -> Left $ "unexpected token 3: " <> t

parseFinishTerminator :: [String] -> Either String (FinishTerminator, [String])
parseFinishTerminator (t:ts) =
  case t of
    "End" -> Right (End, ts)
    _ -> Left $ "unexpected token 4: " <> t

main :: IO ()
main = do
  case parse "Title [ Action Action ] End" of
    Left error -> putStrLn error
    Right diagram -> do
      print diagram
      renderSVG' svgOutputPath svgOptions $ render diagram (p2 (0.0, 0.0))
  -- let diagram =
  --       DrakonDiagram
  --         Title
  --         [ Action
  --         , Fork
  --             (FullBranch
  --               [ Action
  --               , Fork
  --                   (FullBranch [Action, Action])
  --                   (FullBranch [Action, Action, Action])
  --               , Action])
  --             (FullBranch [Action, Action, Action, Action])]
  --         End
