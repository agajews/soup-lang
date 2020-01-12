{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Soup.Value (
    Ident(..),
    Value(..),
    Eval(..),
    Env(..),
    InterpError(..),
    DebugTree(..),
    DebugZipper(..),
    Tree(..),
    Zipper(..),
) where

import Control.Monad.Except
import Control.Monad.Identity
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

newtype Eval a = Eval {
    unwrapEval :: ExceptT InterpError (StateT (Env, DebugZipper) Identity) a
} deriving (Functor,
            Applicative,
            Monad,
            MonadError InterpError,
            MonadState (Env, DebugZipper))

-- (data at current node) (children)
data Tree a = Tree a [Tree a]

-- (data at current node) (parent, whose children exclude this node) (children)
data Zipper a = Zipper (Maybe a) (Maybe (Zipper a)) [Tree a]

type DebugZipper = Zipper (String, String)
type DebugTree = Tree (String, String)

data Env = Env Integer (Map.Map Ident Value)
    deriving (Show)

instance Show DebugTree where
    show t@(Tree (_, program) _) = showEntry 0 t where
        showEntry k (Tree (n, p) children) =
            indent k ++ n ++ " (" ++ (show $ lineno p) ++ ")\n" ++
            (concat $ map (showEntry $ k + 1) children)
        indent k = concat $ replicate k "| "
        lineno p = nlines program - nlines p + 1
        nlines = length . lines

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
