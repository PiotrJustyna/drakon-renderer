module ParserV2 where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Char

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser parsingFunction) = parsingFunction

conditionalSingleCharacterParser :: (Char -> Bool) -> Parser Char
conditionalSingleCharacterParser predicate = Parser (\case
    [] -> Nothing
    (x:xs) -> if predicate x then Just (x, xs) else Nothing)

matchingCharacter :: Char -> Parser Char
matchingCharacter x = conditionalSingleCharacterParser (== x)

matchingString :: String -> Parser String
matchingString [] = return ""
matchingString (x:xs) = do
    y <- matchingCharacter x
    ys <- matchingString xs
    return (y:ys)

space :: Parser ()
space = do
    _ <- Control.Applicative.many $ conditionalSingleCharacterParser Data.Char.isSpace
    return ()

token :: Parser a -> Parser a
token parser = do
    space
    value <- parser
    space
    return value

symbol :: String -> Parser String
symbol xs = token $ matchingString xs

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser parsingFunction) =
        Parser (\x -> case parsingFunction x of
            Nothing -> Nothing
            Just (parsingFunctionResult, remainingInput) -> Just (f parsingFunctionResult, remainingInput))

instance Control.Applicative.Applicative Parser where
    -- pure :: a -> Parser a
    pure applicativeInput =
        Parser (\x -> Just (applicativeInput, x))

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) (Parser firstParsingFunction) nextParser =
        Parser (\x -> case firstParsingFunction x of
            Nothing -> Nothing
            Just (firstParsingFunctionResult, remainingInput) -> parse (fmap firstParsingFunctionResult nextParser) remainingInput)

instance Control.Applicative.Alternative Parser where
    -- empty :: Parser a
    empty = Parser (const Nothing)

    -- (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) (Parser parsingFunction1) (Parser parsingFunction2) =
        Parser (\x -> case parsingFunction1 x of
            Nothing -> parsingFunction2 x
            parsingFunction1Result -> parsingFunction1Result)

instance Control.Monad.Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (Parser parsingFunction) sequentialAction =
        Parser (\x -> case parsingFunction x of
            Nothing -> Nothing
            Just (parsingFunctionResult, remainingInput) -> parse (sequentialAction parsingFunctionResult) remainingInput)
