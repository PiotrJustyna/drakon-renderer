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
  | ForkBlock Fork

data Fork = Fork
  { question :: SkewerBlock
  , left :: Branch
  , right :: Branch
  }

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

instance Renderer Fork where
  render fork@Fork {question = q, left = l, right = r} origin@(P (V2 x y)) =
    render q origin
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
  widthInUnits Fork {question = _, left = l, right = r} =
    widthInUnits l + widthInUnits r
  heightInUnits Fork {question = _, left = l, right = r} =
    heightInUnits Question + max (heightInUnits l) (heightInUnits r)

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
  render (ForkBlock x) origin = render x origin
  widthInUnits Action = 1.0
  widthInUnits Question = 1.0
  widthInUnits (ForkBlock x) = widthInUnits x
  heightInUnits Action = 1.0
  heightInUnits Question = 1.0
  heightInUnits (ForkBlock x) = heightInUnits x

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
    maximum $ (widthInUnits startTerminator) : (map widthInUnits skewerBlocks) ++ [widthInUnits finishTerminator]
  heightInUnits (DrakonDiagram startTerminator skewerBlocks finishTerminator) =
    sum $ (heightInUnits startTerminator) : (map heightInUnits skewerBlocks) ++ [heightInUnits finishTerminator]

main :: IO ()
main = do
  let diagram =
        DrakonDiagram
          Title
          [ Action
          , ForkBlock
              Fork
                { question = Question
                , left =
                    FullBranch
                      [ Action
                      , ForkBlock
                          Fork
                            { question = Question
                            , left = FullBranch [Action, Action]
                            , right = FullBranch [Action, Action, Action]
                            }
                      , Action
                      ]
                , right = FullBranch [Action, Action, Action, Action]
                }
          ]
          End
  putStrLn $ "diagram total width in units: " <> show (widthInUnits diagram)
  putStrLn $ "diagram total height in units: " <> show (heightInUnits diagram)
  renderSVG' svgOutputPath svgOptions $ render diagram (p2 (0.0, 0.0))
