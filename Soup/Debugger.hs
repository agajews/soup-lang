module Soup.Debugger (
    emptyTree,
) where

import Soup.Value

import Control.Monad.State
import Control.Monad.Except

emptyTree :: a -> Zipper a
emptyTree x = Zipper Nothing Nothing [Tree x []]

pushTree :: Zipper a -> Maybe (Zipper a)
pushTree (Zipper y parent (Tree x children : rest)) = Just $ Zipper (Just x) (Just $ Zipper y parent rest) children
pushTree _ = Nothing

popTree :: Zipper a -> Maybe (Zipper a)
popTree (Zipper (Just y) (Just (Zipper x parent children')) children) = Just $ Zipper x parent (Tree y children : children')
popTree _ = Nothing
    
addChild :: a -> Zipper a -> Zipper a
addChild x (Zipper y parent children) = Zipper y parent (Tree x [] : children)

pushDebug :: Eval ()
pushDebug = do
    (env, tree) <- get
    case pushTree tree of
        Just tree' -> put (env, tree')
        Nothing -> throwError InvalidPushTree

popDebug :: Eval ()
popDebug = do
    (env, tree) <- get
    case popTree tree of
        Just tree' -> put (env, tree')
        Nothing -> throwError InvalidPopTree

logParser :: String -> String -> Eval ()
logParser name program = do
    (env, tree) <- get
    put (env, addChild (name, program) tree)
