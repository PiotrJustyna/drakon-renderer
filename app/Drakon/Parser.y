{
module Parser where
import Data.Char
import Lexer
import Drakon.Content
import Drakon.ID
import Drakon.SkewerBlock
import Diagrams.Prelude (Point(..), V2(..), p2)
}

%name                                                                               diagram
%tokentype                                                                          { Token }
%monad                                                                              { P } { thenP } { returnP }

%token
  block                                                                             { TokenBlock $$ }
  soloId                                                                            { TokenSoloIdentifier $$ }
  lBranch                                                                           { TokenLeftBranch }
  rBranch                                                                           { TokenRightBranch }
  '{'                                                                               { TokenOCB }
  '}'                                                                               { TokenCCB }

%%

prods : {- empty -}                                                                 { [[]] }
        | block lBranch '{' prods soloId  '}' rBranch '{' prods soloId '}'          { soloForkLR $1 $4 $5 $9 $10 -- TODO: also always 1D }
        | block lBranch '{' prods soloId  '}' rBranch '{' prods '}'                 { soloForkL $1 $4 $5 $9 -- TODO: also always 1D }
        | block lBranch '{' prods '}' rBranch '{' prods soloId '}'                  { soloForkR $1 $4 $8 $9 -- TODO: also always 1D }
        | block lBranch '{' prods '}' rBranch '{' prods '}'                         { soloFork $1 $4 $8 -- TODO: also always 1D }
        | block                                                                     { [[toAction $1]] }
        | prods block lBranch '{' prods soloId '}' rBranch '{' prods soloId '}'     { appendForkLR $1 $2 $5 $6 $10 $11 -- TODO: also always 1D }
        | prods block lBranch '{' prods soloId '}' rBranch '{' prods '}'            { appendForkL $1 $2 $5 $6 $10 -- TODO: also always 1D }
        | prods block lBranch '{' prods '}' rBranch '{' prods soloId '}'            { appendForkR $1 $2 $5 $9 $10 -- TODO: also always 1D }
        | prods block lBranch '{' prods '}' rBranch '{' prods '}'                   { appendFork $1 $2 $5 $9 -- TODO: also always 1D }
        | prods block                                                               { insertAction $2 $1 }
        | soloId  '{' prods '}'                                                     { soloSkewer $1 $3 -- TODO: also always 1D }
        | soloId  '{' prods soloId '}'                                              { soloSkewer' $1 $3 $4 -- TODO: also always 1D }
        | prods soloId  '{' prods '}'                                               { appendSkewer $1 $2 $4 -- TODO: adjust production rules to only allow 1D prods here }
        | prods soloId  '{' prods soloId '}'                                        { appendSkewer' $1 $2 $4 $5 -- TODO: adjust production rules to only allow 1D prods here }

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
