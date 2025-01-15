module Main where

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
  show CyclicStartWithParameters = visualBlockNoEntry "CyclicStartWithParameters"

-- TODO:
-- * we need a distinction between words and letters
-- * data type representation of words
--   * valent points
--   * dependents
data Fork = Fork
  { forkContent :: String
  , left :: DiagramBlock
  , right :: DiagramBlock
  }

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
  show Headline = visualBlock "Headline"
  show Address = visualBlock "Address"
  show Action = visualBlock "Action"
  show Shelf = visualBlock "Shelf"
  show Choice = visualBlock "Choice"
  show Case = visualBlock "Case"
  show BeginOfForLoop = visualBlock "BeginOfForLoop"
  show EndOfForLoop = visualBlock "EndOfForLoop"
  show Output = visualBlock "Output"
  show Input = visualBlock "Input"
  show Insertion = visualBlock "Insertion"
  show Pause = visualBlock "Pause"
  show StartTimer = visualBlock "StartTimer"
  show ParallelProcess = visualBlock "ParallelProcess"
  show Comment = visualBlock "Comment"
  show (ForkBlock Fork {forkContent = text, left = _, right = _}) = forkBlockVisual text
  show Switch = visualBlock "Switch"
  show ArrowLoop = visualBlock "ArrowLoop"
  show SwitchLoop = visualBlock "SwitchLoop"
  show ForLoop = visualBlock "ForLoop"
  show WaitLoop = visualBlock "WaitLoop"
  show TimerDrivenAction = visualBlock "TimerDrivenAction"
  show TimerDrivenShelf = visualBlock "TimerDrivenShelf"
  show TimerDrivenFork = visualBlock "TimerDrivenFork"
  show TimerDrivenSwitch = visualBlock "TimerDrivenSwitch"
  show TimerDrivenArrowLoop = visualBlock "TimerDrivenArrowLoop"
  show TimerDrivenSwitchLoop = visualBlock "TimerDrivenSwitchLoop"
  show TimerDrivenForLoop = visualBlock "TimerDrivenForLoop"
  show TimerDrivenWaitLoop = visualBlock "TimerDrivenWaitLoop"
  show TimerDrivenOutput = visualBlock "TimerDrivenOutput"
  show TimerDrivenInput = visualBlock "TimerDrivenInput"
  show TimerDrivenInsertion = visualBlock "TimerDrivenInsertion"
  show TimerDrivenStartTimer = visualBlock "TimerDrivenStartTimer"
  show TimerDrivenParallelProcess = visualBlock "TimerDrivenParallelProcess"

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
  show Diagram {blocks = x} = foldl (\accu singleBlock -> accu <> "\n" <> show singleBlock) "" x

visualBlockLength :: Int
visualBlockLength = 11

fill :: Int -> Char -> String
fill 0 _ = ""
fill limit fillingCharacter = fillingCharacter : fill (limit - 1) fillingCharacter

visualBlockCore :: String -> String
visualBlockCore content =
  fill visualBlockLength '-'
    <> "\n| "
    <> content
    <> fill (visualBlockLength - 2 - 1 - length content) ' '
    <> "|\n"
    <> fill visualBlockLength '-'

forkBlockVisual :: String -> String
forkBlockVisual content =
  fill (visualBlockLength `div` 2) ' '
    <> "|\n"
    <> fill visualBlockLength '-'
    <> "\n| "
    <> content
    <> fill (visualBlockLength - 2 - 1 - length content) ' '
    <> "|"
    <> fill 3 '-'
    <> "+\n"
    <> fill visualBlockLength '-'
    <> fill 3 ' '
    <> "|\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"
    <> fill (3 + visualBlockLength `div` 2) ' '
    <> "|\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"
    <> fill (3 + visualBlockLength `div` 2) ' '
    <> "|\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "V"
    <> fill (3 + visualBlockLength `div` 2) ' '
    <> "V\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"
    <> fill (3 + visualBlockLength `div` 2) ' '
    <> "|\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"
    <> fill (3 + visualBlockLength `div` 2) '-'
    <> "+"

visualBlock :: String -> String
visualBlock content =
  fill (visualBlockLength `div` 2) ' '
    <> "|\n"
    <> visualBlockCore content
    <> "\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"

visualBlockNoEntry :: String -> String
visualBlockNoEntry content =
  visualBlockCore content <> "\n" <> fill (visualBlockLength `div` 2) ' ' <> "|"

visualBlockNoExit :: String -> String
visualBlockNoExit content = fill (visualBlockLength `div` 2) ' ' <> "|\n" <> visualBlockCore content

sampleDiagram :: Diagram
sampleDiagram =
  Diagram
    { blocks =
        TerminatorDiagramBlock Title
          : SkewerDiagramBlock Action
          : SkewerDiagramBlock
              (ForkBlock
                 (Fork
                    { forkContent = "f"
                    , left = SkewerDiagramBlock Action
                    , right = SkewerDiagramBlock Action
                    }))
          : SkewerDiagramBlock Action
          : [TerminatorDiagramBlock End]
    }

main :: IO ()
main = do
  putStrLn "Sample drakon diagram:"
  print sampleDiagram
