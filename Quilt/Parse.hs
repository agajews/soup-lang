{-# LANGUAGE FlexibleContexts #-}

module Quilt.Parse (
    parse,
) where

import Quilt.Value
import Quilt.Eval

import Control.Monad.Except
import Control.Monad.State

-- localState :: Eval a -> Eval (a, Env)
-- localState v = do
--     startEnv <- get
--     x <- v
--     endEnv <- get
--     put startEnv
--     return (x, endEnv)

extractParsing :: Value -> Eval [(Value, String)]
extractParsing (ListVal (ListVal [v, StringVal s'] : rest)) = do
    l <- extractParsing (ListVal rest)
    return $ (v, s') : l
extractParsing (ListVal []) = return []
extractParsing _ = throwError InvalidRule

runRule :: String -> Value -> Eval (Value, String)
runRule s v = eval (FuncCall v [StringVal s]) >>= extractParsing >>= fork

extractRules :: Value -> Eval [Value]
extractRules (ListVal [l@(ListVal _)]) = extractRules l
extractRules (ListVal (x : rest)) = do
    l <- extractRules (ListVal rest)
    return $ x:l
extractRules (ListVal []) = return []
extractRules _ = throwError InvalidType

parse :: String -> Value -> Eval (Value, String)
parse s v = extractRules v >>= fork >>= runRule s
