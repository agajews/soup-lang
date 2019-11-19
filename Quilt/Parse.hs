{-# LANGUAGE FlexibleContexts #-}

module Quilt.Parse (
    parse,
) where

import Quilt.Value
import Quilt.Eval

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace

extractParsing :: Value -> Eval [Value]
extractParsing (ListVal l) = return l
extractParsing _ = throwError InvalidRule

evalRule :: Value -> [Value] -> Eval [Value]
evalRule r args = eval (FuncCall r args) >>= extractParsing

runRule :: Value -> String -> Value -> Eval [(Value, Env)]
runRule f s r = do
    startEnv <- get
    ps <- evalRule r [StringVal s, f]
    endEnv <- get
    put startEnv
    return $ map (\v -> (v, endEnv)) ps

extractRules :: Value -> Eval [Value]
extractRules (ListVal [l@(ListVal _)]) = extractRules l
extractRules (ListVal (x : rest)) = do
    l <- extractRules (ListVal rest)
    return (x:l)
extractRules (ListVal []) = return []
extractRules _ = throwError InvalidType

parse :: Value -> String -> Value -> Eval [Value]
parse f s rs = do
    rules <- extractRules rs
    ends <- mapM (runRule f s) rules
    case concat ends of
        [(v, env)] -> put env >> return [v]
        [] -> return []
        _ -> throwError AmbiguousParsing
