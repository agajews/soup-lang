module Soup.Debugger (
    emptyTree,
    rootTree,
    pushDebug,
    popDebug,
    logParser,
) where

import Soup.Value

import Data.Maybe

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace

emptyTree :: a -> Zipper a
emptyTree x = Zipper Nothing Nothing [Tree x []]

pushTree :: Zipper a -> Zipper a
pushTree (Zipper y parent (Tree x children : rest)) =
    Zipper (Just x) (Just $ Zipper y parent rest) children
pushTree z = z

popTree :: Zipper a -> Zipper a
popTree (Zipper (Just y) (Just (Zipper x parent children')) children) =
    Zipper x parent (Tree y children : children')
popTree z = z

rootTree :: Zipper a -> Tree a
rootTree (Zipper _ Nothing [x])  = x
rootTree z@(Zipper _ (Just _) _) = rootTree $ popTree z
rootTree _                       = undefined

addChild :: a -> Zipper a -> Zipper a
addChild x (Zipper y parent children) = Zipper y parent (Tree x [] : children)

pushDebug :: Eval ()
pushDebug = do
    (env, tree) <- get
    put (env, pushTree tree)

popDebug :: Eval ()
popDebug = do
    (env, tree) <- get
    put (env, popTree tree)

logParser :: String -> String -> Eval ()
logParser name program = do
    (env, tree) <- get
    put (env, addChild (name, program) tree)
