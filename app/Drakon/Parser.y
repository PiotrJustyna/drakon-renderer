{
module Parser (diagram, Block) where
import Data.Char
import Lexer
}

%name                     diagram
%tokentype                { Token }
%error                    { parseError }

%token
  block                   { TokenBlock $$ }
  leftBranchIdentifier    { TokenLeftBranchIdentifier $$ }
  rightBranchIdentifier   { TokenRightBranchIdentifier $$ }
  soloIdentifier          { TokenSoloIdentifier $$ }
  '{'                     { TokenOCB }
  '}'                     { TokenCCB }

%%

prods : {- empty -}       { [] }
        | block           { [Block $1] }
        | prods block     { (Block $2) : $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Block
  = Block String
  deriving Show
}
