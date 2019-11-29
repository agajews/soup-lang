module Quilt.Parser (
    Parser(..),
    parserToVal,
    contToVal,
    liftEval,
    catchFail,
    parseType,
    parseString,
    parseIf,
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

import Debug.Trace

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

instance Alternative Parser where
    empty = Parser $ \s c -> return []
    p <|> q = Parser $ \s c -> liftM2 (++) (runParser p s c) (runParser q s c)

liftEval :: Eval a -> Parser a
liftEval m = Parser $ \s c -> do
    x <- m
    c x s

parserToVal :: String -> Parser Value -> Value
parserToVal name p = PrimFunc $ \args -> do
    args <- mapM eval args
    case args of
        [StringVal s, c] -> do
            l <- runParser p s $ \v s' -> do
                y <- eval $ FuncCall c [v, StringVal s']
                case y of
                    ListVal l' -> return l'
                    _ -> throwError InvalidContinuation
            return $ ListVal l
        _ -> throwError $ InvalidArguments' name args

contToVal :: String -> (Value -> String -> Eval [Value]) -> Value
contToVal name c = PrimFunc $ \args -> do
    case args of
        [v, StringVal s] -> do
            l <- c v s
            return $ ListVal l
        _ -> throwError $ InvalidArguments' name args

parseType :: Ident -> Parser Value
parseType n = Parser $ \s c -> do
    rs <- eval (Variable n)
    parse s [(rs, contToVal "--continuation--" c)]

parseString :: String -> Parser ()
parseString m = Parser $ \s c -> if isPrefixOf m s
    then c () $ drop (length m) s
    else return []

parseIf :: (Char -> Bool) -> Parser Char
parseIf p = Parser $ \s c -> case s of
    x:s' -> if p x then c x s' else return []
    _ -> return []

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

parseMany' :: (Show a) => Parser a -> Parser [a]
parseMany' p = catchFail (return []) (parseMany p)

parseMany :: (Show a) => Parser a -> Parser [a]
parseMany p = do
    x <- p
    xs <- parseMany' p
    return (x:xs)

parseInterspersed :: (Show a) => Parser a -> Parser b -> Parser [a]
parseInterspersed p sep = do
    x <- p
    xs <- parseMany' $ sep >> p
    return (x:xs)

parseInterspersed' :: (Show a) => Parser a -> Parser b -> Parser [a]
parseInterspersed' p sep = catchFail (return []) (parseInterspersed p sep)
