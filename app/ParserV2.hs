module ParserV2 where

newtype Parser a = P (String -> Maybe (a, String))