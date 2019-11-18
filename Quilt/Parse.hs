{-# LANGUAGE FlexibleContexts #-}

module Quilt.Parse (
    parse,
) where

import Quilt.Value
import Quilt.Eval

import Control.Monad.Except
import Control.Monad.State

extractParsing :: Value -> Eval [(Value, String)]
extractParsing (ListVal (ListVal [v, StringVal s'] : rest)) = do
    l <- extractParsing (ListVal rest)
    return $ (v, s') : l
extractParsing (ListVal []) = return []
extractParsing _ = throwError InvalidRule

-- evalRule :: Value -> [Value] -> Eval [(Value, String)]
-- evalRule r args = eval (FuncCall r args) >>= extractParsing

runRule :: ((Value, String) -> Eval [(Value, String)]) -> String -> Value -> Eval [(Value, String, Env)]
runRule f s r = do
    startEnv <- get
    ps <- eval (FuncCall r [StringVal s]) >>= extractParsing
    ends <- mapM f ps
    endEnv <- get
    put startEnv
    return $ map (\(v, s) -> (v, s, endEnv)) $ concat ends

extractRules :: Value -> Eval [Value]
extractRules (ListVal [l@(ListVal _)]) = extractRules l
extractRules (ListVal (x : rest)) = do
    l <- extractRules (ListVal rest)
    return $ x:l
extractRules (ListVal []) = return []
extractRules _ = throwError InvalidType

-- runRule :: (a -> String -> Eval [(b, String)]) -> String -> (String -> Eval [(a, String)]) -> Eval [(b, String, Env)]
-- runRule f s r = do
--     startEnv <- get
--     ps <- r s
--     ends <- mapM (\(v, s) -> f v s) ps
--     endEnv <- get
--     put startEnv
--     return $ map (\(v, s) -> (v, s, endEnv)) $ concat ends

-- parse :: (a -> String -> Eval [(b, String)]) -> String -> [String -> Eval [(a, String)]] -> Eval [b]
-- parse f s rs = do
--     ends <- mapM (runRule f s) rs
--     case filter (\(_, s, _) -> null s) $ concat ends of
--         [(v, _, env)] -> put env >> return [v]
--         [] -> return []
--         _ -> throwError AmbiguousParsing

parse :: ((Value, String) -> Eval [(Value, String)]) -> String -> Value -> Eval [Value]
parse f s rs = do
    rules <- extractRules rs
    ends <- mapM (runRule f s) rules
    case filter (\(_, s, _) -> null s) $ concat ends of
        [(v, _, env)] -> put env >> return [v]
        [] -> return []
        _ -> throwError AmbiguousParsing
