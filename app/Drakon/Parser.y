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
        | prods block lBranch '{' prods soloId '}' rBranch '{' prods soloId '}'     { [toFork $2 (ConnectedSkewerBlocks (head $5) (Just (ID $6))) (ConnectedSkewerBlocks (head $10) (Just (ID $11))) : (head $1)] <> (tail $1) }
        | prods block lBranch '{' prods soloId '}' rBranch '{' prods '}'            { [(toFork $2 (ConnectedSkewerBlocks (head $5) (Just (ID $6))) (ConnectedSkewerBlocks (head $10) Nothing)) : (head $1)] <> (tail $1) }
        | prods block lBranch '{' prods '}' rBranch '{' prods soloId '}'            { [(toFork $2 (ConnectedSkewerBlocks (head $5) Nothing) (ConnectedSkewerBlocks (head $9) (Just (ID $10)))) : (head $1)] <> (tail $1) }
        | prods block lBranch '{' prods '}' rBranch '{' prods '}'                   { [(toFork $2 (ConnectedSkewerBlocks (head $5) Nothing) (ConnectedSkewerBlocks (head $9) Nothing)) : (head $1)] <> (tail $1) }
        | prods block                                                               { insertAction $2 $1 }
        | soloId  '{' prods '}'                                                     { soloSkewer $1 $3 -- TODO: also always 1D }
        | soloId  '{' prods soloId '}'                                              { soloSkewer' $1 $3 $4 -- TODO: also always 1D }
        | prods soloId  '{' prods '}'                                               { appendSkewer $1 $2 $4 -- TODO: adjust production rules to only allow 1D prods here }
        | prods soloId  '{' prods soloId '}'                                        { appendSkewer' $1 $2 $4 $5 -- TODO: adjust production rules to only allow 1D prods here }

{

insertAction :: String -> [[SkewerBlock]] -> [[SkewerBlock]]
insertAction actionToken blocks =
  [(toAction actionToken) : (head blocks)] <> (tail blocks)

soloSkewer :: String -> [[SkewerBlock]] -> [[SkewerBlock]]
soloSkewer headlineToken newSkewers =
  [(head newSkewers) <> [toHeadline headlineToken]] <> (tail newSkewers)

soloSkewer' :: String -> [[SkewerBlock]] -> String -> [[SkewerBlock]]
soloSkewer' headlineToken newSkewers addressToken =
  [(toAddress addressToken) : (head newSkewers) <> [toHeadline headlineToken]] <> (tail newSkewers)

appendSkewer :: [[SkewerBlock]] -> String -> [[SkewerBlock]] -> [[SkewerBlock]]
appendSkewer existingSkewers headlineToken newSkewers =
  existingSkewers <> [(head newSkewers) <> [toHeadline headlineToken]]

appendSkewer' :: [[SkewerBlock]] -> String -> [[SkewerBlock]] -> String -> [[SkewerBlock]]
appendSkewer' existingSkewers headlineToken newSkewers addressToken =
  existingSkewers <> [(toAddress addressToken) : (head newSkewers) <> [toHeadline headlineToken]]

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
