module Quilt.Parser (
    Parser(..),
    parserToVal,
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

import Debug.Trace

newtype ParserB a = ParserB { runParserB :: String -> Eval [(a, String)] }

instance Monad ParserB where
    return x = ParserB $ \s -> return [(x, s)]
    p >>= f = ParserB $ \s -> do
        l <- runParserB p s
        xs <- sequence [runParserB (f x) s' | (x, s') <- l]
        return $ concat xs

instance Applicative ParserB where
    pure = return
    (<*>) = ap

instance Functor ParserB where
    fmap = liftM

newtype Parser a = Parser { unwrapParser :: (a -> ParserB Value) -> ParserB Value }

instance Monad Parser where
    return x = Parser $ \c -> c x
    p >>= f = Parser $ \c -> unwrapParser p $ \x -> unwrapParser (f x) c

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM


runParser :: Parser a -> String -> (a -> ParserB Value) -> Eval [(Value, String)]
runParser p s c = runParserB (unwrapParser p c) s

liftParserB :: ParserB a -> Parser a
liftParserB p = Parser $ \c -> p >>= c

lowerParser :: Parser Value -> ParserB Value
lowerParser p = unwrapParser p return

liftEval :: Eval a -> Parser a
liftEval m = liftParserB $ ParserB $ \s -> do
    x <- m
    return [(x, s)]

parsingToVal :: [(Value, String)] -> Value
parsingToVal l = ListVal $ map (\(v, s') -> ListVal [v, StringVal s']) l

parserBToVal :: ParserB Value -> Value
parserBToVal p = PrimFunc $ \x -> case x of
    [StringVal s] -> do
        l <- runParserB p s
        return $ parsingToVal l
    _ -> throwError InvalidArguments

parserToVal :: Parser Value -> Value
parserToVal = parserBToVal . lowerParser

contToVal :: (Value -> ParserB Value) -> Value
contToVal c = PrimFunc $ \x -> case x of
    [v, StringVal s] -> do
        l <- runParserB (c v) s
        return $ parsingToVal l
    _ -> throwError InvalidArguments

parseType :: Ident -> Parser Value
parseType n = traceShow ("parsing ", n) $ Parser $ \c -> ParserB $ \s -> do
    rs <- eval (Variable n)
    l <- parse (contToVal c) s rs
    return $ map (\v -> (v, "")) l

simpleParser :: (String -> Eval [(a, String)]) -> Parser a
simpleParser = liftParserB . ParserB

parseString :: String -> Parser String
parseString m = simpleParser $ \s -> if isPrefixOf m s
    then return [(m, drop (length m) s)]
    else return []

parseWhile' :: (Char -> Bool) -> Parser String
parseWhile' p = simpleParser $ \s -> case span p s of
    (m, s') -> return [(m, s')]

parseWhile :: (Char -> Bool) -> Parser String
parseWhile p = simpleParser $ \s -> case span p s of
    ([], _) -> return []
    (m, s') -> return [(m, s')]

catchFail :: Parser a -> Parser a -> Parser a
catchFail p' p = Parser $ \c -> ParserB $ \s -> do
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
