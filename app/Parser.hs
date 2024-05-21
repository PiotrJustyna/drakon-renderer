module Parser where

import qualified Control.Applicative
import qualified Data.Char

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p =
    P (\x -> case parse p x of
      [] -> []
      [(xa, out)] -> [(f xa, out)]
      _ -> [])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\input -> [(x, input)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\x -> case parse pf x of
    [] -> []
    [(f, out)] -> parse (fmap f px) out)

instance Monad Parser where
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  (P p) >>= f = P (\input -> case p input of
    [] -> []
    [(x, out)] -> parse (f x) out)

instance Control.Applicative.Alternative Parser where
  -- empty :: Parser a
  empty = P (const [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  (P p1) <|> (P p2) = P (\input -> case p1 input of
    [] -> p2 input
    [(x, out)] -> [(x, out)])

parse :: Parser a -> String -> [(a, String)]
parse (P parsingFunction) = parsingFunction

item :: Parser Char
item = P (\case
            [] -> []
            (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat f = do
  x <- item
  if f x then return x else Control.Applicative.empty
  -- but the below also works:
  -- if f x then P (\input -> [(x, input)]) else Control.Applicative.empty

digit :: Parser Char
digit = sat Data.Char.isDigit

lower :: Parser Char
lower = sat Data.Char.isLower

alphanum :: Parser Char
alphanum = sat Data.Char.isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  _ <- char x
  _ <- string xs
  return (x:xs)

identifier' :: Parser String
identifier' = do
  x <- lower
  xs <- Control.Applicative.many alphanum
  return (x:xs)

identifier :: Parser String
identifier = token identifier'

naturalNumber' :: Parser Int
naturalNumber' = do
  xs <- Control.Applicative.some digit
  return (read xs)

naturalNumber :: Parser Int
naturalNumber = token naturalNumber'

naturalNumbers :: Parser [Int]
naturalNumbers = do _ <- symbol "["
                    x <- naturalNumber
                    xs <- Control.Applicative.many (do symbol ","
                                                       naturalNumber)
                    _ <- symbol "]"
                    return (x:xs)

symbol :: String -> Parser String
symbol xs = token (string xs)

integer :: Parser Int
integer = do
  _ <- char '-'
  n <- naturalNumber
  return (-n)
  Control.Applicative.<|>
  naturalNumber

space :: Parser ()
space = do
  _ <- Control.Applicative.many (sat Data.Char.isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v