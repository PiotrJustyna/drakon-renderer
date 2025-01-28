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

class Renderer a where
  render :: a -> Point V2 Double -> Diagram B
  widthInUnits :: a -> Double
  heightInUnits :: a -> Double

data Terminator
  = Title
  | CyclicStart
  | End
  | TitleWithParameters
  | CyclicStartWithParameters

instance Renderer Terminator where
  render Title origin =
    position
      [ ( origin
        , (roundedRect
             (widthInUnits Title * defaultBlockWidth)
             (heightInUnits Title * defaultBlockHeight * 0.5)
             0.5
             # lw thick
             # lc lineColour
             # translate
                 (r2 (defaultBlockWidth * 0.5, defaultBlockHeight * (-0.5))))
            <> (rect'
                  (widthInUnits Title * defaultBlockWidth)
                  (heightInUnits Title * defaultBlockHeight)
                  # lw veryThin))
      ]
  render _ _ = mempty
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

data ValentPoint =
  ValentPoint

instance Renderer ValentPoint where
  render ValentPoint origin =
    position
      [ ( origin
        , rect'
            (widthInUnits Action * defaultBlockWidth)
            (heightInUnits Action * defaultBlockHeight)
            # lw veryThin)
      ]
  widthInUnits _ = 1.0
  heightInUnits _ = 1.0

data Fork = Fork
  { question :: SkewerBlock
  , left :: Either ValentPoint [SkewerBlock] -- this deserves a dedicated type (to allow for Renderer instance)
  , right :: Either ValentPoint [SkewerBlock] -- same here
  }

renderEither :: Either ValentPoint [SkewerBlock] -> Point V2 Double -> Diagram B
renderEither (Left ValentPoint) = render ValentPoint
renderEither (Right skewerBlocks) = render Action

heightInUnits' :: Either ValentPoint [SkewerBlock] -> Double
heightInUnits' (Left ValentPoint) = heightInUnits ValentPoint
heightInUnits' (Right skewerBlocks) = sum $ map heightInUnits skewerBlocks

instance Renderer Fork where
  render fork@Fork {question = q, left = l, right = r} origin@(P (V2 x y)) =
    render q origin
      <>
       renderEither
         l
         (P (V2 x (y - heightInUnits Question * defaultBlockHeight)))
      <>
       renderEither
         r
         (P (V2
               (x + widthInUnits Question * defaultBlockWidth)
               (y - heightInUnits Question * defaultBlockHeight)))
      <> position
           [ ( origin
             , rect'
                 (widthInUnits fork * defaultBlockWidth)
                 (heightInUnits fork * defaultBlockHeight)
                 # lw veryThin)
           ]
  widthInUnits _ = 2.0
  heightInUnits Fork {question = _, left = l, right = _} = heightInUnits Question + heightInUnits' l

data SkewerBlock
  = Action
  | Question
  | ForkBlock Fork

instance Renderer SkewerBlock where
  render Action origin =
    let iconHeight = heightInUnits Action * defaultBlockHeight * 0.5
     in position
          [ ( origin
            , rect' (widthInUnits Action * defaultBlockWidth) iconHeight
                # lw thick
                # lc lineColour
                # translate (r2 (0.0, iconHeight * (-0.5)))
                <> (rect'
                      (widthInUnits Action * defaultBlockWidth)
                      (heightInUnits Action * defaultBlockHeight)
                      # lw veryThin))
          ]
  render Question origin =
    let iconHeight = heightInUnits Action * defaultBlockHeight * 0.5
     in position
          [ ( origin
            , hex' (widthInUnits Action * defaultBlockWidth) iconHeight
                # lw thick
                # lc lineColour
                # translate (r2 (0.1, iconHeight * (-0.5)))
                <> (rect'
                      (widthInUnits Action * defaultBlockWidth)
                      (heightInUnits Action * defaultBlockHeight)
                      # lw veryThin))
          ]
  render (ForkBlock x) origin = render x origin
  widthInUnits Action = 1.0
  widthInUnits Question = 1.0
  widthInUnits (ForkBlock x) = widthInUnits x
  heightInUnits Action = 1.0
  heightInUnits Question = 1.0
  heightInUnits (ForkBlock x) = heightInUnits x

data DiagramBlock
  = TerminatorDiagramBlock Terminator
  | SkewerDiagramBlock SkewerBlock

instance Renderer DiagramBlock where
  render (TerminatorDiagramBlock x) = render x
  render (SkewerDiagramBlock x) = render x
  widthInUnits (TerminatorDiagramBlock x) = widthInUnits x
  widthInUnits (SkewerDiagramBlock x) = widthInUnits x
  heightInUnits (TerminatorDiagramBlock x) = heightInUnits x
  heightInUnits (SkewerDiagramBlock x) = heightInUnits x

newtype DrakonDiagram = DrakonDiagram
  { blocks :: [DiagramBlock]
  }

instance Renderer DrakonDiagram where
  render DrakonDiagram {blocks = allBlocks} (P (V2 x y)) =
    fst
      $ foldl
          (\accu singleBlock ->
             ( fst accu <> render singleBlock (P (V2 x (snd accu)))
             , snd accu - heightInUnits singleBlock * defaultBlockHeight))
          (mempty, y)
          allBlocks
  widthInUnits DrakonDiagram {blocks = allBlocks} =
    maximum $ map widthInUnits allBlocks
  heightInUnits DrakonDiagram {blocks = allBlocks} =
    sum $ map heightInUnits allBlocks

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

defaultBlockWidth :: Double
defaultBlockWidth = 1.0

defaultBlockHeight :: Double
defaultBlockHeight = 1.0

lineColour :: Colour Double
lineColour = sRGB (34.0 / 255.0) (69.0 / 255.0) (57.0 / 255.0)

main :: IO ()
main = do
  let diagram =
        DrakonDiagram
          { blocks =
              [ TerminatorDiagramBlock Title
              , TerminatorDiagramBlock Title
              , SkewerDiagramBlock Action
              , SkewerDiagramBlock
                  (ForkBlock
                     Fork
                       { question = Question
                       , left = Right [Action, Action, Action]
                       , right = Left ValentPoint
                       })
              , TerminatorDiagramBlock Title
              , TerminatorDiagramBlock Title
              , TerminatorDiagramBlock Title
              , TerminatorDiagramBlock Title
              , TerminatorDiagramBlock Title
              , TerminatorDiagramBlock Title
              ]
          }
  putStrLn $ "diagram total width in units: " <> show (widthInUnits diagram)
  putStrLn $ "diagram total height in units: " <> show (heightInUnits diagram)
  renderSVG' svgOutputPath svgOptions $ render diagram (p2 (0.0, 0.0))
