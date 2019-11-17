{-# LANGUAGE FlexibleContexts #-}

module Quilt.Parse (
    parse,
) where

import Quilt.Value
import Quilt.Eval

import Control.Monad.Except
import Control.Monad.State

localState :: Eval a -> (Eval a, Env)
localState v = do
    startEnv <- get
    x <- v
    endEnv <- get
    put startEnv
    return (x, endEnv)

extractParsing :: Value -> [(Value, String)]
extractParsing (ListVal (ListVal [v, StringVal s'] : rest)) = do
    l <- extractParsing (ListVal rest)
    return $ (v, s') : l
extractParsing (ListVal []) = return []
extractParsing _ = throwError InvalidRule

runRule :: String -> Value -> Eval [(Value, Env, String)]
runRule s v = do
    (p, env) <- localState $ eval (FuncCall v [StringVal s]) >>= extractParsing
    return [(v, env, s') | (v, s') <- p]

extractRules :: Value -> Eval [Value]
extractRules (ListVal l@(ListVal _)) = extractRules l
extractRules (ListVal x : rest) = x : extractRules $ ListVal rest
extractRules (ListVal []) = return []
extractRules _ = throwError InvalidType

parse :: String -> Value -> Eval [(Value, Env, String)]
parse s v = do
    rules <- extractRules v
    parsings <- mapM (runRule s) rules
    return $ concat parsings

