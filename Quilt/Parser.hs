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

newtype Parser a = Parser { runParser :: String -> Eval [(a, Env, String)] }

instance Monad Parser where
    return x = Parser $ \s -> do
        env <- get
        return [(x, env, s)]

    p >>= f = Parser $ \s -> runParser p s >>= bindParser
        
instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

parseFail :: Parser a
parseFail = Parser $ \s -> return []

parseReturn :: a -> String -> Eval [(a, Env, String)]
parseReturn x s' = do
    env <- get
    return [(x, env, s')]

bindParser :: (a -> Parser b) -> [(a, Env, String)] -> Eval [(b, Env, String)]
bindParser f l = liftM concat . sequence $ do
    (x, env, s') <- l
    return $ do
        startEnv <- get
        put env
        t <- runParser (f x) s'
        put startEnv
        return t

liftEval :: Eval a -> Parser a
liftEval m = Parser $ \s -> do
    x <- m
    env <- get
    return [(x, env, s)]

parserToValue :: Parser Value -> Value
parserToValue p = PrimFunc $ \x -> case x of
    [StringVal s] -> ListVal $ runParser p s >>= mapM f
    _ -> throwError InvalidArguments
    where f (v, env, s') = put env >> return ListVal [v, StringVal s']

catchFail :: Parser a -> Parser b -> (a -> Parser b) -> Parser b
catchFail p failure success = Parser $ \s -> runParser p s >>= handleFailure s where
    handleFailure s l
        | null l = runParser failure s
        | otherwise = bindParser success l

parseType :: Ident -> Parser Value
parseType n = Parser $ \s -> do
    v <- eval (Variable n)
    parse s v

-- localState :: Parser a -> Parser a
-- localState p = do
--     startEnv <- get
--     x <- p
--     put startEnv
--     return x

parseString :: String -> Parser ()
parseString m = Parser $ \s -> if isPrefixOf m s
    then parseReturn () $ drop (length m) s
    else return []

parseWhile' :: (Char -> Bool) -> Parser String
parseWhile' p = Parser $ \s -> let (m, s') = span p s in parseReturn m s'

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
