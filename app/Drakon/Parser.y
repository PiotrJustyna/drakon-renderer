{
module Parser where
import Data.Char
import Lexer
}

%name                                                                     diagram
%tokentype                                                                { Token }
%monad                                                                    { P } { thenP } { returnP }

%token
  block                                                                   { TokenBlock $$ }
  soloIdentifier                                                          { TokenSoloIdentifier $$ }
  leftBranch                                                              { TokenLeftBranch }
  rightBranch                                                             { TokenRightBranch }
  '{'                                                                     { TokenOCB }
  '}'                                                                     { TokenCCB }

%%

prods : {- empty -}                                                       { [] }
        | block                                                           { [ActionBlock $1] }
        | block leftBranch '{' prods '}' rightBranch '{' prods '}'        { [ForkBlock $1 $4 $8] }
        | prods block                                                     { (ActionBlock $2) : $1 }
        | prods block leftBranch '{' prods '}' rightBranch '{' prods '}'  { (ForkBlock $2 $5 $9) : $1 }

{
happyError = \tks i -> error ("Parse error in line " ++ show (i::Int) ++ ".\n")

data ParseResult a
  = ParseOk a
  | ParseFail String

type P a = Int -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \l ->
  case m l of
    ParseFail s -> ParseFail s
    ParseOk a -> k a l

returnP :: a -> P a
returnP a = \l -> ParseOk a

-- 1 "action 1"
-- 2 "action 2"
-- 3 "fork"
--   L {
--     3l1 "left branch - action 1"
--     3l2 "left branch - action 2"
--     3l3 "left branch - action 3"
--   }
--   R {
--     3r1 "right branch - action 1"
--     3r2 "right branch - action 2"
--   }

data Block
  = ActionBlock String
  | ForkBlock String [Block] [Block]
  deriving Show

buildForkBlock :: Block -> Block
buildForkBlock _ = ForkBlock "" [] []
}
