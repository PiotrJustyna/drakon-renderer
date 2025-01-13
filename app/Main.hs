module Main where

data Terminator
  = Title
  | CyclicStart
  | End
  | TitleWithParameters
  | CyclicStartWithParameters

instance Show Terminator where
  show Title = "T"
  show CyclicStart = "CS"
  show End = "E"
  show TitleWithParameters = "TWP"
  show CyclicStartWithParameters = "CSWP"

data Block
  = TerminatorBlock Terminator
  | SkewerBlock

instance Show Block where
  show SkewerBlock = "|"
  show (TerminatorBlock t) = show t

newtype Diagram = Diagram
  { blocks :: [Block]
  }

instance Show Diagram where
  show Diagram {blocks = x} = foldl (\accu singleBlock -> accu <> "\n" <> show singleBlock) "" x

sampleDiagram :: Diagram
sampleDiagram = Diagram {blocks = (TerminatorBlock Title) : SkewerBlock : SkewerBlock : [(TerminatorBlock End)]}

main :: IO ()
main = do
  putStrLn "Hello, drakon!"
  putStrLn "Sample drakon diagram:"
  print sampleDiagram
