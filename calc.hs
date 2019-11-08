module Calc (
    Node(..),
    parseNum
) where

import Data.Char
import Control.Monad
import Control.Applicative

-- Abstract definitions here

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) s = f s

extract :: Parser a -> String -> [a]
extract (Parser f) s = [x | (x, s') <- f s, null s']

instance Monad Parser where
    return x = Parser $ \s -> [(x, s)]
    p >>= f = Parser $ \s -> concat [parse (f x) s' | (x, s') <- parse p s]

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

instance Alternative Parser where
    empty = Parser $ \s -> []
    Parser f <|> Parser g = Parser $ \s -> f s ++ g s

parseWhile :: (Char -> Bool) -> Parser String
parseWhile pred = Parser $ \s -> case span pred s of
    ([], _) -> []
    (x, s') -> [(x, s')]

parseChar :: Char -> Parser ()
parseChar c = Parser $ \s -> case s of
    (x:xs) -> if x == c then [((), xs)] else []
    _ -> []

-- Real definitions here

data Node = Num Int
          | Plus Node Node
          | Minus Node Node
          | Times Node Node
          deriving (Show)

parseNum :: Parser Node
parseNum = do
    x <- parseWhile isDigit
    return (Num (read x))

parse0 :: Parser Node
parse0 = parseNum <|> parseAtomic

parseTimes :: Parser Node
parseTimes = do
    x <- parse0
    parseChar '*'
    y <- parse1
    return (Times x y)

parse1 :: Parser Node
parse1 = parseTimes <|> parse0

parsePlus :: Parser Node
parsePlus = do
    x <- parse1
    parseChar '+'
    y <- parse2
    return (Plus x y)

parseMinus :: Parser Node
parseMinus = do
    x <- parse1
    parseChar '-'
    y <- parse2
    return (Minus x y)

parse2 :: Parser Node
parse2 = parsePlus <|> parseMinus <|> parse1

parseAtomic :: Parser Node
parseAtomic = do
    parseChar '('
    x <- parse2
    parseChar ')'
    return x

parseCalc :: String -> [Node]
parseCalc s = extract parse2 s
