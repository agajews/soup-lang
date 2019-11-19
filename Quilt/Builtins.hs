{-# LANGUAGE FlexibleContexts #-}

module Quilt.Builtins (
    builtins,
) where

import Quilt.Value
import Quilt.Eval
import Quilt.Parse
import Quilt.Parser

import Control.Monad.Except

builtins :: [(String, Value)]
builtins = [("+", function $ intBinOp (+)),
            ("-", function $ intBinOp (-)),
            ("parse-str", function parseStr),
            ("parse", function doParse)]

function :: ([Value] -> Eval Value) -> Value
function f = PrimFunc $ \args -> mapM eval args >>= f

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> Eval Value
intBinOp op [IntVal x, IntVal y] = return $ IntVal (op x y)
intBinOp _ _ = throwError InvalidArguments

parseStr :: [Value] -> Eval Value
parseStr [StringVal m, StringVal s, v] = eval $ FuncCall (parserToVal p) [StringVal s, v]
    where p = parseString m >> return (StringVal m)
parseStr _ = throwError InvalidArguments

doParse :: [Value] -> Eval Value
doParse [StringVal s, ListVal l] = do
    tuples <- mapM getTuples l
    parsings <- parse s tuples
    return $ ListVal parsings
    where getTuples (ListVal [x, y]) = return (x, y)
          getTuples _ = throwError InvalidArguments
doParse _ = throwError InvalidArguments
