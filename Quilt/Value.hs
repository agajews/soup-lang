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

data InterpError = UnboundVariable Ident
                 | NotAVariable Value
                 | TooFewArgs
                 | TooManyArgs
                 | InvalidRule
                 | InvalidContinuation
                 | InvalidType
                 | InvalidArguments
                 | ParsingError
                 | AmbiguousParsing
    deriving (Show)

newtype Ident = Ident Integer
    deriving (Show, Eq, Ord)

data Value = StringVal String
           | IntVal Integer
           | ListVal [Value]
           | PrimFunc ([Value] -> Eval Value)
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

instance Show Value where
    show (StringVal x) = "StringVal " ++ show x
    show (IntVal x) = "IntVal " ++ show x
    show (ListVal x) = "ListVal " ++ show x
    show (PrimFunc _) = "PrimFunc"
    show (Lambda ps b) = "Lambda " ++ show ps ++ " " ++ show b
    show (Variable n) = show n
    show (FuncCall v args) = "FuncCall " ++ show v ++ " " ++ show args
