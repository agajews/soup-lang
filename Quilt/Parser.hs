module Quilt.Parser (
    Parser(..),
    parserToVal,
    contToVal,
    liftEval,
    parseType,
    parseString,
    parseWhile,
    parseWhile',
    parseMany,
    parseMany',
    parseInterspersed,
    parseInterspersed',
) where

import Quilt.Value
import Quilt.Eval
import Quilt.Parse

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

import Data.List

newtype Parser a = Parser {
    runParser :: String -> (a -> String -> Eval [Value]) -> Eval [Value]
}

instance Monad Parser where
    return x = Parser $ \s c -> c x s
    p >>= f = Parser $ \s c -> runParser p s $ \x s -> runParser (f x) s c

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

liftEval :: Eval a -> Parser a
liftEval m = Parser $ \s c -> do
    x <- m
    c x s

parserToVal :: Parser Value -> Value
parserToVal p = PrimFunc $ \x -> case x of
    [StringVal s, c] -> do
        l <- runParser p s $ \v s' -> do
            y <- eval $ FuncCall c [v, StringVal s']
            case y of
                ListVal l' -> return l'
                _ -> throwError InvalidContinuation
        return $ ListVal l
    _ -> throwError InvalidArguments

contToVal :: (Value -> String -> Eval [Value]) -> Value
contToVal c = PrimFunc $ \x -> case x of
    [v, StringVal s] -> do
        l <- c v s
        return $ ListVal l
    _ -> throwError InvalidArguments

parseType :: Ident -> Parser Value
parseType n = Parser $ \s c -> do
    rs <- eval (Variable n)
    parse (contToVal c) s rs

parseString :: String -> Parser ()
parseString m = Parser $ \s c -> if isPrefixOf m s
    then c () $ drop (length m) s
    else return []

parseWhile' :: (Char -> Bool) -> Parser String
parseWhile' p = Parser $ \s c -> case span p s of
    (m, s') -> c m s'

parseWhile :: (Char -> Bool) -> Parser String
parseWhile p = Parser $ \s c -> case span p s of
    ([], _) -> return []
    (m, s') -> c m s'

catchFail :: Parser a -> Parser a -> Parser a
catchFail p' p = Parser $ \s c -> do
    l <- runParser p s c
    if null l
    then runParser p' s c
    else return l

parseMany' :: Parser a -> Parser [a]
parseMany' p = catchFail (return []) (parseMany p)

parseMany :: Parser a -> Parser [a]
parseMany p = do
    x <- p
    xs <- parseMany p
    return (x:xs)

parseInterspersed :: Parser a -> Parser b -> Parser [a]
parseInterspersed p sep = do
    x <- p
    xs <- parseMany' $ sep >> p
    return (x:xs)

parseInterspersed' :: Parser a -> Parser b -> Parser [a]
parseInterspersed' p sep = catchFail (return []) (parseInterspersed p sep)
