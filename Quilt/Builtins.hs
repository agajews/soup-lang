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

builtins :: [(String, Value)]
builtins = [("+", function $ intBinOp (+)),
            ("-", function $ intBinOp (-)),
            ("*", function $ intBinOp (*)),
            ("parse-str", function parseStr),
            ("parse", function parseFun),
            ("gen-var", function genVar),
            ("do", function doFun),
            ("set!", PrimFunc set),
            ("cons", function cons),
            ("list", function list),
            ("eval", function evalFun),
            ("apply", PrimFunc apply),
            ("quote", PrimFunc quote)]

function :: ([Value] -> Eval Value) -> Value
function f = PrimFunc $ \args -> mapM eval args >>= f

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> Eval Value
intBinOp op [IntVal x, IntVal y] = return $ IntVal (op x y)
intBinOp _ _ = throwError InvalidArguments

parseStr :: [Value] -> Eval Value
parseStr [StringVal m, StringVal s, v] = eval $ FuncCall (parserToVal p) [StringVal s, v]
    where p = parseString m >> return (StringVal m)
parseStr _ = throwError InvalidArguments

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

set :: [Value] -> Eval Value
set [Variable n, v] = do
    v' <- eval v
    setVar n v'
    return v'
set _ = throwError InvalidArguments

cons :: [Value] -> Eval Value
cons [ListVal l, v] = return $ ListVal (v:l)
cons _ = throwError InvalidArguments

list :: [Value] -> Eval Value
list args = return $ ListVal args

evalFun :: [Value] -> Eval Value
evalFun [v] = eval v
evalFun _ = throwError InvalidArguments

apply :: [Value] -> Eval Value
apply (f:args) = do
    args' <- mapM eval args
    eval $ FuncCall f args'

quote :: [Value] -> Eval Value
quote [v] = return v
quote _ = throwError InvalidArguments
