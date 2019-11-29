{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Quilt.Value (
    Ident(..),
    Value(..),
    Eval(..),
    Env(..),
    InterpError(..),
    runEval,
) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as Map
import Data.List

data InterpError = UnboundVariable Ident
                 | NotAVariable Value
                 | NotAFunction Value
                 | TooFewArgs
                 | TooManyArgs
                 | InvalidRule
                 | InvalidContinuation
                 | InvalidType
                 | InvalidArguments
                 | InvalidArguments' String [Value]
                 | ParsingError
                 | AmbiguousParsing [Value]
    deriving (Show)

data Ident = Ident String Integer
    deriving (Show)

instance Eq Ident where
    (Ident _ x) == (Ident _ y) = x == y

instance Ord Ident where
    (Ident _ x) <= (Ident _ y) = x <= y

data Value = StringVal String
           | IntVal Integer
           | ListVal [Value]
           | PrimFunc String ([Value] -> Eval Value)
           | Lambda [Ident] Value
           | Variable Ident
           | FuncCall Value [Value]

newtype Eval a = Eval { unwrapEval :: StateT Env (ExceptT InterpError Identity) a }
    deriving (Functor,
              Applicative,
              Monad,
              MonadError InterpError,
              MonadState Env)

runEval :: Env -> Eval a -> Either InterpError (a, Env)
runEval env x = runIdentity (runExceptT (runStateT (unwrapEval x) env))

data Env = Env Integer (Map.Map Ident Value)
    deriving (Show)

instance Show Value where
    show (StringVal x) = show x
    show (IntVal x) = show x
    show (ListVal l) = if null l
        then "(list)"
        else "(list " ++ intercalate " " (map show l) ++ ")"
    show (PrimFunc name _) = "prim:" ++ name
    show (Lambda ps b) = "lambda"
    show (Variable (Ident name x)) = "var:" ++ name ++ ":" ++ show x
    show (FuncCall v args) = if null args
        then "(" ++ show v ++ ")"
        else "(" ++ show v ++ " " ++ intercalate " " (map show args) ++ ")"
