{-# LANGUAGE FlexibleContexts #-}

module Quilt.Parse (
    parse,
) where

import Quilt.Value
import Quilt.Eval

import Control.Monad.Except
import Control.Monad.State

localState :: Eval a -> Eval a
localState v = do
    env <- get
    case runEval env v of
        Right v -> return v
        Left err -> throwError err

runRule :: String -> Eval Value -> Eval [(Eval Value, String)]
runRule s v = do
    p <- localState parsing
    return [(parsing >> return v, s') | (v, s') <- p]
    where
        parsing = v >>= (\x -> eval $ FuncCall x [StringVal s]) >>= extractParsing
        extractParsing :: Value -> Eval [(Value, String)]
        extractParsing (ListVal (ListVal [v, StringVal s'] : rest)) = do
            l <- extractParsing (ListVal rest)
            return $ (v, s') : l
        extractParsing (ListVal []) = return []
        extractParsing _ = throwError InvalidRule

extractRules :: Eval Value -> Eval [Eval Value]
extractRules v = do
    rs <- localState rules 
    return [rules >> return r | r <- concat rs]
    where
        extractType (ListVal (ListVal ts : rest)) = do
            l <- extractType $ ListVal rest
            return $ ts : l
        extractType (ListVal []) = return []
        extractType _ = throwError InvalidType
        rules = v >>= eval >>= extractType

parse :: String -> Eval Value -> Eval [(Eval Value, String)]
parse s v = do
    rules <- extractRules v
    parsings <- mapM (runRule s) rules
    return $ concat parsings

