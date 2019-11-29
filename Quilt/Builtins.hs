{-# LANGUAGE FlexibleContexts #-}

module Quilt.Builtins (
    builtins,
) where

import Quilt.Value
import Quilt.Env
import Quilt.Eval
import Quilt.Parse
import Quilt.Parser

import Control.Monad.Except

import Data.List

import Debug.Trace

builtins :: [(String, Value)]
builtins = [function "+" $ intBinOp (+),
            function "-" $ intBinOp (-),
            function "*" $ intBinOp (*),
            function "parse" parseFun,
            function "gen-var" genVar,
            function "do" doFun,
            function "cons" cons,
            function "list" list,
            function "eval" evalFun,
            function "starts-with" startsWith,
            function "length" lengthFun,
            function "drop" dropFun,
            macro "set!" set,
            macro "if" ifFun,
            macro "apply" apply,
            macro "quote" quote]

function :: String -> ([Value] -> Eval Value) -> (String, Value)
function name f = (name, PrimFunc name func) where
    func args = traceShow (name, args) $ mapM eval args >>= wrapInvalidArgs f name

macro :: String -> ([Value] -> Eval Value) -> (String, Value)
macro name f = (name, PrimFunc name $ \args -> wrapInvalidArgs f name args)

wrapInvalidArgs :: ([Value] -> Eval Value) -> String -> [Value] -> Eval Value
wrapInvalidArgs f name args = catchError (f args) $ \err -> case err of
    InvalidArguments -> throwError $ InvalidArguments' name args
    _ -> throwError err

-- Functions

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> Eval Value
intBinOp op [IntVal x, IntVal y] = return $ IntVal (op x y)
intBinOp _ _ = throwError $ InvalidArguments

genVar :: [Value] -> Eval Value
genVar [] = genIdent >>= return . Variable
genVar _ = throwError InvalidArguments

parseFun :: [Value] -> Eval Value
parseFun [StringVal s, ListVal l] = do
    tuples <- mapM getTuples l
    parsings <- parse s tuples
    return $ ListVal parsings
    where getTuples (FuncCall x [y]) = return (x, y)
          getTuples _ = throwError InvalidArguments
parseFun _ = throwError InvalidArguments

doFun :: [Value] -> Eval Value
doFun args@(x:_) = return $ last args
doFun _ = throwError InvalidArguments

cons :: [Value] -> Eval Value
cons [v, ListVal l] = return $ ListVal (v:l)
cons _ = throwError InvalidArguments

list :: [Value] -> Eval Value
list args = return $ ListVal args

evalFun :: [Value] -> Eval Value
evalFun [v] = eval v
evalFun _ = throwError InvalidArguments

startsWith :: [Value] -> Eval Value
startsWith [StringVal s, StringVal m] = return $ if isPrefixOf s m
    then StringVal m
    else ListVal []
startsWith _ = throwError InvalidArguments

dropFun :: [Value] -> Eval Value
dropFun [IntVal n, ListVal l] = return $ ListVal $ drop' n l where
    drop' 0 l = l
    drop' n (x:xs) = drop' (n-1) xs
    drop' _ [] = []
dropFun _ = throwError InvalidArguments

lengthFun :: [Value] -> Eval Value
lengthFun [ListVal l] = return $ IntVal $ toInteger $ length l
lengthFun _ = throwError InvalidArguments

-- Macros

set :: [Value] -> Eval Value
set [Variable n, v] = do
    v' <- eval v
    setVar n v'
    return v'
set _ = throwError InvalidArguments

ifFun :: [Value] -> Eval Value
ifFun [cond, thenExpr, elseExpr] = do
    b <- eval cond
    case b of
        ListVal [] -> eval elseExpr
        _ -> eval thenExpr

apply :: [Value] -> Eval Value
apply (f:args) = do
    args' <- mapM eval args
    eval $ FuncCall f args'

quote :: [Value] -> Eval Value
quote [v] = return v
quote _ = throwError InvalidArguments

