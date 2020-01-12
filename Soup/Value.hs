{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Soup.Value (
    Ident(..),
    Value(..),
    Eval(..),
    Env(..),
    InterpError(..),
    DebugTree(..),
    Tree(..),
    Zipper(..),
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
                 | MismatchedArgs
                 | MismatchedArgs' [Ident] [Value]
                 | InvalidRule Value [Value]
                 | InvalidContinuation
                 | InvalidType Value
                 | InvalidArguments
                 | InvalidArguments' String [Value]
                 | InvalidPushTree
                 | InvalidPopTree
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

newtype Eval a = Eval { unwrapEval :: StateT (Env, DebugTree) (ExceptT InterpError Identity) a }
    deriving (Functor,
              Applicative,
              Monad,
              MonadError InterpError,
              MonadState (Env, DebugTree))

data Tree a = Tree a [Tree a]

data Zipper a = Zipper (Maybe a) (Maybe (Zipper a)) [Tree a]

type DebugTree = Zipper (String, String)

runEval :: (Env, DebugTree) -> Eval a -> Either InterpError (a, (Env, DebugTree))
runEval (env, debugTree) x = runIdentity (runExceptT (runStateT (unwrapEval x) (env, debugTree)))

data Env = Env Integer (Map.Map Ident Value)
    deriving (Show)

instance Show Value where
    show (StringVal x) = show x
    show (IntVal x) = show x
    show (ListVal l) = if null l
        then "(list)"
        else "(list " ++ intercalate " " (map show l) ++ ")"
    show (PrimFunc name _) = name
    show (Lambda ps b) = "lambda"
    show (Variable (Ident name x)) = "var:" ++ name ++ ":" ++ show x
    show (FuncCall v args) = if null args
        then "(" ++ show v ++ ")"
        else "(" ++ show v ++ " " ++ intercalate " " (map show args) ++ ")"
