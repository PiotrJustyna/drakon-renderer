{
module Parser where
import Data.Char
import Lexer
import Drakon.Content
import Drakon.ID
import Drakon.SkewerBlock
import Diagrams.Prelude (Point(..), V2(..), p2)
}

%name                                                                                                   diagram
%tokentype                                                                                              { Token }
%monad                                                                                                  { P } { thenP } { returnP }

%token
  block                                                                                                 { TokenBlock $$ }
  soloIdentifier                                                                                        { TokenSoloIdentifier $$ }
  leftBranch                                                                                            { TokenLeftBranch }
  rightBranch                                                                                           { TokenRightBranch }
  '{'                                                                                                   { TokenOCB }
'}'                                                                                                     { TokenCCB }

%%

prods : {- empty -}                                                                                     { [] }
        | block                                                                                         { [toAction $1] }
        | block leftBranch '{' prods soloIdentifier '}' rightBranch '{' prods soloIdentifier '}'        { [toFork $1 (ConnectedSkewerBlocks $4 Nothing) (ConnectedSkewerBlocks $9 (Just (ID $10)))] }
        | block leftBranch '{' prods soloIdentifier '}' rightBranch '{' prods '}'                       { [toFork $1 (ConnectedSkewerBlocks $4 Nothing) (ConnectedSkewerBlocks $9 Nothing)] }
        | block leftBranch '{' prods '}' rightBranch '{' prods soloIdentifier '}'                       { [toFork $1 (ConnectedSkewerBlocks $4 Nothing) (ConnectedSkewerBlocks $8 (Just (ID $9)))] }
        | block leftBranch '{' prods '}' rightBranch '{' prods '}'                                      { [toFork $1 (ConnectedSkewerBlocks $4 Nothing) (ConnectedSkewerBlocks $8 Nothing)] }
        | prods block                                                                                   { (toAction $2) : $1 }
        | prods block leftBranch '{' prods soloIdentifier '}' rightBranch '{' prods soloIdentifier '}'  { (toFork $2 (ConnectedSkewerBlocks $5 Nothing) (ConnectedSkewerBlocks $10 (Just (ID $11)))) : $1 }
        | prods block leftBranch '{' prods soloIdentifier '}' rightBranch '{' prods '}'                 { (toFork $2 (ConnectedSkewerBlocks $5 Nothing) (ConnectedSkewerBlocks $10 Nothing)) : $1 }
        | prods block leftBranch '{' prods '}' rightBranch '{' prods soloIdentifier '}'                 { (toFork $2 (ConnectedSkewerBlocks $5 Nothing) (ConnectedSkewerBlocks $9 (Just (ID $10)))) : $1 }
        | prods block leftBranch '{' prods '}' rightBranch '{' prods '}'                                { (toFork $2 (ConnectedSkewerBlocks $5 Nothing) (ConnectedSkewerBlocks $9 Nothing)) : $1 }

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
