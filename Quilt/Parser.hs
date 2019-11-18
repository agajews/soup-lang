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

newtype ParserC a = ParserC { runParserC :: (a -> ParserB Value) -> ParserB Value }

instance Monad ParserC where
    return x = ParserC $ \c -> c x
    p >>= f = ParserC $ \c -> runParserC p $ \x -> runParserC (f x) c

instance Applicative ParserC where
    pure = return
    (<*>) = ap

instance Functor ParserC where
    fmap = liftM

type Parser = ParserC Value

runParser :: Parser -> (Value -> ParserB Value) -> String -> Eval [(Value, String)]
runParser p c s = runParserB (runParserC p c) s

liftParserB :: ParserB Value -> Parser
liftParserB p = ParserC $ \c -> p >>= c

lowerParser :: Parser -> ParserB Value
lowerParser p = runParserC p return

liftEval :: Eval Value -> Parser
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

parserToVal :: Parser -> Value
parserToVal = parserBToVal . lowerParser

contToVal :: (Value -> ParserB Value) -> Value
contToVal c = PrimFunc $ \x -> case x of
    [v, StringVal s] -> do
        l <- runParserB (c v) s
        return $ parsingToVal l
    _ -> throwError InvalidArguments

catchFail :: Parser -> Parser -> (Value -> Parser) -> Parser
catchFail p failure success = ParserC $ \c -> ParserB $ \s -> runParser p c s >>= handleFailure s c where
    handleFailure s c l
        | null l = runParser failure c s
        | otherwise = liftM concat . sequence $ [runParser (success x) c s' | (x, s') <- l]

parseType :: Ident -> Parser
parseType n = ParserC $ \c -> ParserB $ \s -> do
    rs <- eval (Variable n)
    l <- parse (contToVal c) s rs
    return $ map (\v -> (v, "")) l

simpleParser :: (String -> Eval [(Value, String)]) -> Parser
simpleParser = liftParserB . ParserB

parseString :: String -> Parser
parseString m = simpleParser $ \s -> if isPrefixOf m s
    then return [(StringVal m, drop (length m) s)]
    else return []

parseWhile' :: (Char -> Bool) -> Parser
parseWhile' p = simpleParser $ \s -> case span p s of
    (m, s') -> return [(StringVal m, s')]

parseWhile :: (Char -> Bool) -> Parser
parseWhile p = simpleParser $ \s -> case span p s of
    ([], _) -> return []
    (m, s') -> return [(StringVal m, s')]

fromListVal :: Value -> [Value]
fromListVal (ListVal l) = l
fromListVal _ = undefined

parseMany' :: Parser -> Parser
parseMany' p = catchFail p (return $ ListVal []) $ \x -> do
    xs <- parseMany' p
    return $ ListVal (x:fromListVal xs)

parseMany :: Parser -> Parser
parseMany p = do
    x <- p
    xs <- parseMany p
    return $ ListVal (x:fromListVal xs)

parseInterspersed :: Parser -> Parser -> Parser
parseInterspersed p sep = do
    x <- p
    l <- parseMany' $ sep >> p
    return $ ListVal (x:fromListVal l)

parseInterspersed' :: Parser -> Parser -> Parser
parseInterspersed' p sep = catchFail p (return $ ListVal []) $ \x -> do
    l <- parseMany' $ sep >> p
    return $ ListVal (x:fromListVal l)
