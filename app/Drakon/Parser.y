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
  soloId                                                                                        { TokenSoloIdentifier $$ }
  lBranch                                                                                            { TokenLeftBranch }
  rBranch                                                                                           { TokenRightBranch }
  '{'                                                                               { TokenOCB }
  '}'                                                                               { TokenCCB }

%%

prods : {- empty -}                                                                 { [[]] }
        | block lBranch '{' prods soloId  '}' rBranch '{' prods soloId '}'          { [[toFork $1 (ConnectedSkewerBlocks (head $4) (Just (ID $5))) (ConnectedSkewerBlocks (head $9) (Just (ID $10)))]] }
        | block lBranch '{' prods soloId  '}' rBranch '{' prods '}'                 { [[toFork $1 (ConnectedSkewerBlocks (head $4) (Just (ID $5))) (ConnectedSkewerBlocks (head $9) Nothing)]] }
        | block lBranch '{' prods '}' rBranch '{' prods soloId '}'                  { [[toFork $1 (ConnectedSkewerBlocks (head $4) Nothing) (ConnectedSkewerBlocks (head $8) (Just (ID $9)))]] }
        | block lBranch '{' prods '}' rBranch '{' prods '}'                         { [[toFork $1 (ConnectedSkewerBlocks (head $4) Nothing) (ConnectedSkewerBlocks (head $8) Nothing)]] }
        | block                                                                     { [[toAction $1]] }
        | prods block lBranch '{' prods soloId '}' rBranch '{' prods soloId '}'     { $1 <> [[toFork $2 (ConnectedSkewerBlocks (head $5) (Just (ID $6))) (ConnectedSkewerBlocks (head $10) (Just (ID $11)))]] -- TODO: tail }
        | prods block lBranch '{' prods soloId '}' rBranch '{' prods '}'            { $1 <> [[toFork $2 (ConnectedSkewerBlocks (head $5) (Just (ID $6))) (ConnectedSkewerBlocks (head $10) Nothing)]] -- TODO: tail }
        | prods block lBranch '{' prods '}' rBranch '{' prods soloId '}'            { [(toFork $2 (ConnectedSkewerBlocks (head $5) Nothing) (ConnectedSkewerBlocks (head $9) (Just (ID $10)))) : (head $1)] <> (tail $1) }
        | prods block lBranch '{' prods '}' rBranch '{' prods '}'                   { [(toFork $2 (ConnectedSkewerBlocks (head $5) Nothing) (ConnectedSkewerBlocks (head $9) Nothing)) : (head $1)] <> (tail $1) }
        | prods block                                                               { [(toAction $2) : (head $1)] <> (tail $1) }
        | soloId  '{' prods '}'                                                     { [(toAddress $1) : (head $3) <> [toHeadline $1]] <> (tail $3) }
        | soloId  '{' prods soloId '}'                                              { [(toAddress $4) : (head $3) <> [toHeadline $1]] <> (tail $3) }
        | prods soloId  '{' prods '}'                                               { $1 <> [(toBlankAddress $2) : (head $4) <> [toHeadline $2]] }
        | prods soloId  '{' prods soloId '}'                                        { $1 <> [(toAddress $5) : (head $4) <> [toHeadline $2]] }

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
