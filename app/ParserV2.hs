module ParserV2 where

import qualified Control.Applicative
import qualified Control.Monad

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser parsingFunction) = parsingFunction

singleCharacterParser :: Parser Char
singleCharacterParser = Parser (\case
    [] -> Nothing
    (x:xs) -> Just (x, xs))

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap inputFunction2 (Parser parsingFunction) =
        Parser (\inputString -> case parsingFunction inputString of
            Nothing -> Nothing
            Just (parserResult, outputString) -> Just (inputFunction2 parserResult, outputString))

-- 2024-06-05 PJ:
-- ==============
-- This is needed for Parser to be an instance of:
-- * Alternative
-- * Monad
-- classes.
instance Control.Applicative.Applicative Parser where
    -- pure :: a -> Parser a
    pure applicativeInput =
        Parser (\inputString -> Just (applicativeInput, inputString))

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) (Parser parsingFunction2) parser1 =
        Parser (\inputString -> case parsingFunction2 inputString of
            Nothing -> Nothing
            Just (parser2Result, outputString) -> parse (fmap parser2Result parser1) outputString)

instance Control.Monad.Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (Parser parsingFunction) sequentialAction =
        Parser (\inputString -> case parsingFunction inputString of
            Nothing -> Nothing
            Just (parserResult, outputString) -> parse (sequentialAction parserResult) outputString)