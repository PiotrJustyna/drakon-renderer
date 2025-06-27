{
module Parser (diagram, Block) where
import Data.Char
import Lexer
}

%name                                                           diagram
%tokentype                                                      { Token }
%error                                                          { parseError }

%token
  block                                                         { TokenBlock $$ }
  soloIdentifier                                                { TokenSoloIdentifier $$ }
  leftBranch                                                    { TokenLeftBranch }
  rightBranch                                                   { TokenRightBranch }
  '{'                                                           { TokenOCB }
  '}'                                                           { TokenCCB }

%%

fork : block leftBranch '{' prods '}' rightBranch '{' prods '}' { ForkBlock $1 [ActionBlock "hello from left branch!"] [ActionBlock "hello from right branch!"] }

prods : {- empty -}                                             { [] }
        | block                                                 { [ForkBlock $1] }
        | prods block                                           { (ForkBlock $2) : $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Block
  = ActionBlock String
  | ForkBlock String [Block] [Block]
  deriving Show
}
