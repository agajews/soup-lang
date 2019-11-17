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
import Quilt.Parse

import Control.Monad.Except
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Eval [(a, Env, String)] }

instance Monad Parser where
    return x = Parser $ \s -> do
        env <- get
        [(x, env, s)]

    p >>= f = Parser $ \s -> do
        l <- runParser p s
        return . concat . sequence $ do
            (x, env, s') <- l
            return $ do
                startEnv <- get
                put env
                t <- runParser (f x) s'
                put startEnv
                return t

    fail = Parser $ \s -> return []

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

liftEval :: Eval a -> Parser a
liftEval m = Parser $ \s -> do
    x <- m
    env <- get
    return [(x, env, s)]

parserToValue :: Parser Value -> Value
parserToValue p = PrimFunc $ \x -> case x of
    [StringVal s] -> ListVal $ runParser p s >>= map f
    _ -> throwError InvalidArguments
    where f (v, env, s') = put env >> ListVal [v, StringVal s']

catchFail :: Parser a -> Parser b -> (a -> Parser b) -> Parser b
catchFail p failure success = Parser $ \s -> return $ handleFailure $ runParser p s where
    handleFailure l
        | null l -> failure
        | otherwise -> concat [(runParser $ success x) s' | (x, s') <- l]

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
parseString m = \s -> return $ if isPrefixOf m s
    then [((), drop (length m) s)]
    else []

parseWhile' :: (Char -> Bool) -> Parser String
parseWhile' p = Parser $ \s -> return [span p s]

parseWhile :: (Char -> Bool) -> Parser String
parseWhile p = do
    m <- parseWhile' p
    if null m then fail else return m

parseMany' p = catchFail p (return []) $ \x -> do
    xs <- parseMany' p
    return (x:xs)

parseMany p = do
    x <- p
    xs <- parseMany p
    return (x:xs)

parseInterspersed :: Parser a -> Parser b -> [a]
parseInterspersed p sep = do
    x <- p
    l <- parseMany' $ sep >> p
    return (x:l)

parseInterspersed' :: Parser a -> Parser b -> [a]
parseInterspersed' p sep = catchFail p (return []) $ \x -> do
    l <- parseMany' $ sep >> p
    return (x:l)
