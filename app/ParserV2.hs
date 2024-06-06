module ParserV2 where

import qualified Control.Applicative
import qualified Control.Monad

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser parsingFunction) = parsingFunction

conditionalSingleCharacterParser :: (Char -> Bool) -> Parser Char
conditionalSingleCharacterParser predicate = Parser (\case
    [] -> Nothing
    (x:xs) -> if predicate x then Just (x, xs) else Nothing)

matchingCharacter :: Char -> Parser Char
matchingCharacter x = conditionalSingleCharacterParser (== x)

-- 2024-06-06 PJ:
-- ==============
-- OK, this...
-- I don't fully understand the do notation here.
-- It just looks too simple, I don't see how it connects
-- to the previously defined functions.
-- 2024-06-06 PJ:
-- ==============
-- The types here are also looking messed up...
-- why return a char/string from matchingCharacter x/matchingString xs
-- if we're not using it.
matchingString :: String -> Parser String
matchingString [] = return ""
matchingString (x:xs) = do
    y <- matchingCharacter x
    ys <- matchingString xs
    return (y:ys)

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
    -- 2024-06-06 PJ:
    -- ==============
    -- This is rather complicated to me. Need to remember that f is of type:
    -- String -> Maybe (a -> b, String)
    -- So it's a parser that can maybe return:
    -- * another function that can be fmapped over Parser a
    -- * remaining input
    (<*>) (Parser firstParsingFunction) nextParser =
        Parser (\x -> case firstParsingFunction x of
            Nothing -> Nothing
            Just (firstParsingFunctionResult, remainingInput) -> parse (fmap firstParsingFunctionResult nextParser) remainingInput)

instance Control.Monad.Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (Parser parsingFunction) sequentialAction =
        Parser (\x -> case parsingFunction x of
            Nothing -> Nothing
            Just (parsingFunctionResult, remainingInput) -> parse (sequentialAction parsingFunctionResult) remainingInput)

instance Control.Applicative.Alternative Parser where
    -- empty :: Parser a
    empty = Parser (const Nothing)

    -- (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) (Parser parsingFunction1) (Parser parsingFunction2) =
        Parser (\x -> case parsingFunction1 x of
            Nothing -> parsingFunction2 x
            parsingFunction1Result -> parsingFunction1Result)