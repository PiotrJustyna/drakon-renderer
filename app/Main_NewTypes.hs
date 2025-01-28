module Main where

import Data.Either (isLeft)

data Terminator
  = Title
  | CyclicStart
  | End
  | TitleWithParameters
  | CyclicStartWithParameters

instance Show Terminator where
  show Title = visualBlockNoEntry "Title"
  show CyclicStart = visualBlockNoEntry "CyclicStart"
  show End = visualBlockNoExit "End"
  show TitleWithParameters = visualBlockNoEntry "TitleWithParameters"
  show CyclicStartWithParameters =
    visualBlockNoEntry "CyclicStartWithParameters"

data ValentPoint =
  ValentPoint

instance AsciiShow ValentPoint where
  asciiShow _ = visualBlock "V"

data Fork = Fork
  { forkContent :: String
  -- this deserves a dedicated, more fleshed out type
  , left :: Either ValentPoint [SkewerBlock]
  , right :: Either ValentPoint [SkewerBlock]
  }

class AsciiShow a where
  asciiShow :: a -> Int -> String

instance Show Fork where
  show Fork {forkContent = fC, left = l, right = r} =
    fill (visualBlockLength `div` 2) ' '
      <> "|\n"
      <> fill visualBlockLength '-'
      <> "\n| "
      <> fC
      <> fill (visualBlockLength - 2 - 1 - length fC) ' '
      <> "|"
      <> fill 10 '-'
      <> "+\n"
      <> fill visualBlockLength '-'
      <> fill 10 ' '
      <> "|\n"
      <> fill (visualBlockLength `div` 2) ' '
      <> "|"
      <> fill (10 + visualBlockLength `div` 2) ' '
      <> "|\n"
      <> (if isLeft l
            then asciiShow ValentPoint 0
            else asciiShow Action 0)
      <> fill (10 + visualBlockLength `div` 2) ' '
      <> (if null r
            then "V"
            else "O")
      <> "\n"
      <> fill (visualBlockLength `div` 2) ' '
      <> "|"
      <> fill (10 + visualBlockLength `div` 2) '-'
      <> "+"

data SkewerBlock
  = Headline
  | Address -- letters ->
  | Action
  | Shelf
  | Choice
  | Case
  | BeginOfForLoop
  | EndOfForLoop
  | Output
  | Input
  | Insertion
  | Pause
  | StartTimer
  | ParallelProcess
  | Comment -- <- letters
  | ForkBlock Fork -- words ->
  | Switch
  | ArrowLoop
  | SwitchLoop
  | ForLoop
  | WaitLoop
  | TimerDrivenAction
  | TimerDrivenShelf
  | TimerDrivenFork
  | TimerDrivenSwitch
  | TimerDrivenArrowLoop
  | TimerDrivenSwitchLoop
  | TimerDrivenForLoop
  | TimerDrivenWaitLoop
  | TimerDrivenOutput
  | TimerDrivenInput
  | TimerDrivenInsertion
  | TimerDrivenStartTimer
  | TimerDrivenParallelProcess -- <- words

instance Show SkewerBlock where
  show Headline = visualBlock "Headline" 0
  show Address = visualBlock "Address" 0
  show Action = visualBlock "Action" 0
  show Shelf = visualBlock "Shelf" 0
  show Choice = visualBlock "Choice" 0
  show Case = visualBlock "Case" 0
  show BeginOfForLoop = visualBlock "BeginOfForLoop" 0
  show EndOfForLoop = visualBlock "EndOfForLoop" 0
  show Output = visualBlock "Output" 0
  show Input = visualBlock "Input" 0
  show Insertion = visualBlock "Insertion" 0
  show Pause = visualBlock "Pause" 0
  show StartTimer = visualBlock "StartTimer" 0
  show ParallelProcess = visualBlock "ParallelProcess" 0
  show Comment = visualBlock "Comment" 0
  show (ForkBlock x) = show x
  show Switch = visualBlock "Switch" 0
  show ArrowLoop = visualBlock "ArrowLoop" 0
  show SwitchLoop = visualBlock "SwitchLoop" 0
  show ForLoop = visualBlock "ForLoop" 0
  show WaitLoop = visualBlock "WaitLoop" 0
  show TimerDrivenAction = visualBlock "TimerDrivenAction" 0
  show TimerDrivenShelf = visualBlock "TimerDrivenShelf" 0
  show TimerDrivenFork = visualBlock "TimerDrivenFork" 0
  show TimerDrivenSwitch = visualBlock "TimerDrivenSwitch" 0
  show TimerDrivenArrowLoop = visualBlock "TimerDrivenArrowLoop" 0
  show TimerDrivenSwitchLoop = visualBlock "TimerDrivenSwitchLoop" 0
  show TimerDrivenForLoop = visualBlock "TimerDrivenForLoop" 0
  show TimerDrivenWaitLoop = visualBlock "TimerDrivenWaitLoop" 0
  show TimerDrivenOutput = visualBlock "TimerDrivenOutput" 0
  show TimerDrivenInput = visualBlock "TimerDrivenInput" 0
  show TimerDrivenInsertion = visualBlock "TimerDrivenInsertion" 0
  show TimerDrivenStartTimer = visualBlock "TimerDrivenStartTimer" 0
  show TimerDrivenParallelProcess = visualBlock "TimerDrivenParallelProcess" 0

instance AsciiShow SkewerBlock where
  asciiShow Action offset = visualBlock "Action" offset
  asciiShow _ _ = ""

data DiagramBlock
  = TerminatorDiagramBlock Terminator
  | SkewerDiagramBlock SkewerBlock

instance Show DiagramBlock where
  show (SkewerDiagramBlock x) = show x
  show (TerminatorDiagramBlock x) = show x

newtype Diagram = Diagram
  { blocks :: [DiagramBlock]
  }

instance Show Diagram where
  show Diagram {blocks = x} =
    foldl (\accu singleBlock -> accu <> "\n" <> show singleBlock) "" x

visualBlockLength :: Int
visualBlockLength = 11

fill :: Int -> Char -> String
fill 0 _ = ""
fill limit fillingCharacter =
  fillingCharacter : fill (limit - 1) fillingCharacter

visualBlockCore :: String -> Int -> String
visualBlockCore content offset =
  fill offset ' '
    <> fill visualBlockLength '-'
    <> "\n"
    <> fill offset ' '
    <> "| "
    <> content
    <> fill (visualBlockLength - 2 - 1 - length content) ' '
    <> "|\n"
    <> fill offset ' '
    <> fill visualBlockLength '-'

visualBlock :: String -> Int -> String
visualBlock content offset =
  fill offset ' '
    <> fill (visualBlockLength `div` 2) ' '
    <> "|\n"
    <> visualBlockCore content offset
    <> "\n"
    <> fill offset ' '
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"

visualBlockNoEntry :: String -> String
visualBlockNoEntry content =
  visualBlockCore content 0
    <> "\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"

visualBlockNoExit :: String -> String
visualBlockNoExit content =
  fill (visualBlockLength `div` 2) ' ' <> "|\n" <> visualBlockCore content 0

sampleDiagram :: Diagram
sampleDiagram =
  Diagram
    { blocks =
        TerminatorDiagramBlock Title
          : SkewerDiagramBlock Action
          : SkewerDiagramBlock
              (ForkBlock
                 (Fork
                    { forkContent = "Fork"
                    , left = Left ValentPoint
                    , right = Right [Action]
                    }))
          : SkewerDiagramBlock Action
          : [TerminatorDiagramBlock End]
    }

main :: IO ()
main = do
  putStrLn "Sample drakon diagram:"
  print sampleDiagram
