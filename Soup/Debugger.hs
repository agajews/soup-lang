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

emptyTree :: Zipper a
emptyTree = Zipper [] Nothing []

pushTree :: Zipper a -> Zipper a
pushTree z = Zipper [] (Just z) []

popTree :: Zipper a -> Zipper a
popTree (Zipper xs' (Just (Zipper xs parent children)) children') =
    Zipper xs parent (Tree xs' children' : children)
popTree z = undefined

rootTree :: Zipper a -> Tree a
rootTree (Zipper xs Nothing children) = Tree xs children
rootTree z                            = rootTree $ popTree z

addData :: a -> Zipper a -> Zipper a
addData x (Zipper xs parent children) = Zipper (x : xs) parent children

pushDebug :: Eval ()
pushDebug = do
    (env, debugZip) <- get
    put (env, pushTree debugZip)

popDebug :: Eval ()
popDebug = do
    (env, debugZip) <- get
    put (env, popTree debugZip)

logParser :: String -> String -> Eval ()
logParser name program = do
    (env, debugZip) <- get
    put (env, addData (name, program) debugZip)
