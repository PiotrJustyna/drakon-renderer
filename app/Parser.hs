module Parser where

import qualified Control.Applicative
import qualified Data.Char

import qualified Icon

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

isAllowedDescriptionCharacter :: Char -> Bool
isAllowedDescriptionCharacter x =
  Data.Char.isAlphaNum x
  || Data.Char.isSpace x
  || x == '-'

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  _ <- char x
  _ <- string xs
  return (x:xs)

-- 2024-05-23 PJ:
-- ==============
-- TODO: many but no more than N
iconIdentifier :: Parser String
iconIdentifier = do Control.Applicative.many $ sat Data.Char.isAlphaNum

iconDescription :: Parser String
iconDescription = do
  _ <- symbol "\""
  name <- Control.Applicative.many $ sat isAllowedDescriptionCharacter
  _ <- symbol "\""
  return name

-- 2024-05-23 PJ:
-- ==============
-- I thought about "many but no more than N"
-- but in the end convinced myself it won't be needed.
-- We can always limit the number of spaces using an
-- input size constant.
-- Any input larger than that constant will be ignored.
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

symbol :: String -> Parser String
symbol xs = token (string xs)

iconDefinition' :: Parser Icon.Icon
iconDefinition' =
    do
      _ <- symbol "Title"
      identifier <- token iconIdentifier
      description <- token iconDescription
      return Icon.Icon { Icon.iconText = description, Icon.iconType = Icon.Title}
  Control.Applicative.<|>
    do
      _ <- symbol "Action"
      identifier <- token iconIdentifier
      description <- token iconDescription
      return Icon.Icon { Icon.iconText = description, Icon.iconType = Icon.Action}
  Control.Applicative.<|>
    do
      _ <- symbol "Question"
      identifier <- token iconIdentifier
      description <- token iconDescription
      return Icon.Icon { Icon.iconText = description, Icon.iconType = Icon.Question}
  Control.Applicative.<|>
    do
      _ <- symbol "End"
      identifier <- token iconIdentifier
      description <- token iconDescription
      return Icon.Icon { Icon.iconText = description, Icon.iconType = Icon.End}

iconDefinition :: Parser Icon.Icon
iconDefinition = token iconDefinition'