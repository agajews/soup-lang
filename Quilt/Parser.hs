module Quilt.Parser (
    Parser(..),
    parserToValue,
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

newtype ParserB a = ParserB { runParserB :: String -> Eval [(a, String)] }

instance Monad ParserB where
    return x = ParserB $ \s -> return [(x, s)]
    p >>= f = ParserB $ \s -> do
        l <- runParserB p s
        xs <- sequence [runParserB (f x) s' | (x, s') <- l]
        return $ concat xs
    -- p >>= f = Parser $ \s -> parse (runParser . f) s [runParser p]

instance Applicative ParserB where
    pure = return
    (<*>) = ap

instance Functor ParserB where
    fmap = liftM

newtype Parser a = Parser { runParser :: (a -> ParserB a) -> ParserB a }

instance Monad Parser where
    return x = Parser $ \c -> c x
    p >>= f = Parser $ \c -> runParser p $ \x -> runParser f x c

instance Applicative ParserB where
    pure = return
    (<*>) = ap

instance Functor ParserB where
    fmap = liftM

liftParserB :: ParserB a -> Parser a
liftParserB p = Parser $ \c -> p >>= c

parseFail :: Parser a
parseFail = Parser $ \s -> return []

liftEval :: Eval a -> Parser a
liftEval m = liftParserB $ ParserB $ \s -> do
    x <- m
    return [(x, s)]

parserToValue :: Parser Value -> Value
parserToValue p = PrimFunc $ \x -> case x of
    [StringVal s] -> do
        l <- runParser p s
        return $ ListVal $ map (\(v, s') -> ListVal [v, StringVal s']) l
    _ -> throwError InvalidArguments

contToValue :: (Value -> Parser Value) -> Value
contToValue f = PrimFunc $ \x -> case x of
    [v, StringVal s] -> do
        l <- runParser (f v) s
        return $ ListVal $ map (\(v, s') -> ListVal [v, StringVal s']) l
    _ -> throwError InvalidArguments

catchFail :: Parser a -> Parser b -> (a -> Parser b) -> Parser b
catchFail p failure success = Parser $ \s -> runParser p s >>= handleFailure s where
    handleFailure s l
        | null l = runParser failure s
        | otherwise = liftM concat . sequence $ [runParser (success x) s' | (x, s') <- l]

parseType :: Ident -> (Value -> Parser Value) -> Parser Value
parseType n p = Parser $ \s -> do
    rs <- eval (Variable n)
    l <- parse (contToValue p) s rs
    return $ map (\v -> (v, "")) l

parseString :: String -> Parser ()
parseString m = Parser $ \s -> if isPrefixOf m s
    then return [((), drop (length m) s)]
    else return []

parseWhile' :: (Char -> Bool) -> Parser String
parseWhile' p = Parser $ \s -> return [span p s]

parseWhile :: (Char -> Bool) -> Parser String
parseWhile p = do
    m <- parseWhile' p
    if null m then parseFail else return m

parseMany' p = catchFail p (return []) $ \x -> do
    xs <- parseMany' p
    return (x:xs)

parseMany p = do
    x <- p
    xs <- parseMany p
    return (x:xs)

parseInterspersed :: Parser a -> Parser b -> Parser [a]
parseInterspersed p sep = do
    x <- p
    l <- parseMany' $ sep >> p
    return (x:l)

parseInterspersed' :: Parser a -> Parser b -> Parser [a]
parseInterspersed' p sep = catchFail p (return []) $ \x -> do
    l <- parseMany' $ sep >> p
    return (x:l)
