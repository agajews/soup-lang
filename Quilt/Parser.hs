module Quilt.Parser (
    Parser(..),
    parserToValue,
) where

import Quilt.Value

import Control.Monad.Except
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Monad Parser where
    return x = Parser $ \s -> [(x, s)]
    p >>= f = Parser $ \s -> concat [(runParser $ f x) s' | (x, s') <- runParser p s]

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

parserToValue :: Parser Value -> Value
parserToValue (Parser p) = PrimFunc $ \x -> case x of
    [StringVal s] -> return . ListVal . map (\(v, s') -> ListVal [v, StringVal s']) $ p s
    _ -> throwError InvalidArguments
