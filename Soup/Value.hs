{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Soup.Value (
    Ident(..),
    Value(..),
    Eval(..),
    Env(..),
    EnvMap(..),
    InterpError(..),
    DebugTree,
    DebugZipper,
    Tree(..),
    Zipper(..),
) where

import Control.Monad.Except
import Control.Monad.State

import Data.List
import qualified Data.Map as Map

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
                 | InvalidPushTree DebugZipper
                 | InvalidPopTree DebugZipper
                 | ParsingError
                 | AmbiguousParsing [Value]
    deriving (Show)

data Ident = Ident String Integer
    deriving (Show)

identName :: Ident -> String
identName (Ident name _) = name

instance Eq Ident where
    (Ident _ x) == (Ident _ y) = x == y

instance Ord Ident where
    (Ident _ x) <= (Ident _ y) = x <= y

data Value = StringVal String
           | IntVal Integer
           | ListVal [Value]
           | PrimFunc String ([Value] -> Eval Value)
           | Lambda [Ident] [Integer] Value
           | Variable Ident
           | FuncCall Value [Value]

newtype Eval a = Eval {
    unwrapEval :: ExceptT InterpError (StateT (Env, DebugZipper) IO) a
} deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            MonadError InterpError,
            MonadState (Env, DebugZipper))

-- (data at current node) (children)
data Tree a = Tree a [Tree a]
    deriving (Show)

-- (data at current node) (parent, whose children exclude this node) (children)
data Zipper a = Zipper [a] (Maybe (Zipper a)) [Tree [a]]
    deriving (Show)

type DebugZipper = Zipper (String, String)
type DebugTree = Tree [(String, String)]

data EnvMap = EnvMap (Map.Map Ident (Either EnvMap Value))
    deriving (Show)

-- (global variable count) (local evaluation scope) (the actual env)
data Env = Env Integer [Integer] EnvMap
    deriving (Show)

instance Show Value where
    show (StringVal x) = show x
    show (IntVal x) = show x
    show (ListVal l) = if null l
        then "[]"
        else "[" ++ intercalate " " (map show l) ++ "]"
    show (PrimFunc name _) = name
    show (Lambda ps _ b) = "(/\\ " ++ intercalate " " (map identName ps) ++ " . " ++ show b ++ ")"
    show (Variable (Ident name _)) = "'" ++ name
    show (FuncCall v args) = if null args
        then "(" ++ show v ++ ")"
        else "(" ++ show v ++ " " ++ intercalate " " (map show args) ++ ")"
