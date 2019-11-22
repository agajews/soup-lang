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

extractRules :: Value -> Eval [[Value]]
extractRules (ListVal [l@(ListVal _)]) = do
    rules <- extractRules l
    return $ [] : rules
extractRules (ListVal (x : rest)) = do
    l <- extractRules (ListVal rest)
    return $ case l of
        y:ys -> (x:y):ys
        [] -> [[x]]
extractRules (ListVal []) = return []
extractRules _ = throwError InvalidType

parse :: String -> [(Value, Value)] -> Eval [Value]
parse s l = do
    ends <- forM l $ \(rs, f) -> do
        rules <- extractRules rs
        ends <- liftM (map concat) $ (mapM . mapM) (runRule f s) rules
        return $ case filter (not . null) ends of
            e:_ -> e
            [] -> []
    case concat ends of
        [(v, env)] -> put env >> return [v]
        [] -> return []
        _ -> throwError AmbiguousParsing
