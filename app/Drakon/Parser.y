{
module Parser where
import Data.Char
import Lexer
import Drakon.Content
import Drakon.ID
import Drakon.SkewerBlock
import Diagrams.Prelude (Point(..), V2(..), p2)
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
        | block                                                           { [Action (ID "-1") (p2 (-1.0, -1.0)) (Content $1)] }
        | block leftBranch '{' prods '}' rightBranch '{' prods '}'        { [Action (ID "-1") (p2 (-1.0, -1.0)) (Content $1)] }
        | prods block                                                     { (Action (ID "-1") (p2 (-1.0, -1.0)) (Content $2)) : $1 }
        | prods block leftBranch '{' prods '}' rightBranch '{' prods '}'  { (Action (ID "-1") (p2 (-1.0, -1.0)) (Content $2)) : $1 }

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
}
