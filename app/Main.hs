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

data SkewerBlock
  = Headline
  | Address
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
  | Comment

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
visualBlockLength = 21

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
    visualBlockCore content
    <> "\n"
    <> fill (visualBlockLength `div` 2) ' '
    <> "|"

visualBlockNoExit :: String -> String
visualBlockNoExit content =
    fill (visualBlockLength `div` 2) ' '
    <> "|\n"
    <> visualBlockCore content

sampleDiagram :: Diagram
sampleDiagram =
  Diagram
    { blocks =
        TerminatorDiagramBlock Title
          : SkewerDiagramBlock Action
          : SkewerDiagramBlock BeginOfForLoop
          : SkewerDiagramBlock EndOfForLoop
          : SkewerDiagramBlock ParallelProcess
          : SkewerDiagramBlock Action
          : [TerminatorDiagramBlock End]
    }

main :: IO ()
main = do
  putStrLn "Hello, drakon!"
  putStrLn "Sample drakon diagram:"
  print sampleDiagram
