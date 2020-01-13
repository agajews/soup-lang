{-# LANGUAGE FlexibleContexts #-}

module Soup.Builtins (
    builtins,
) where

import Soup.Debugger
import Soup.Env
import Soup.Eval
import Soup.Parse
import Soup.Value

import Control.Monad.Except

import Data.List

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
            function "cat" cat,
            function "drop" dropFun,
            function "str->list" strToList,
            function "list->str" listToStr,
            function "log-parser" logParserFun,
            macro "set!" set,
            macro "if" ifFun,
            macro "apply" apply,
            macro "quote" quote]

function :: String -> ([Value] -> Eval Value) -> (String, Value)
function name f = (name, PrimFunc name func) where
    func args = mapM eval args >>= wrapInvalidArgs f name

macro :: String -> ([Value] -> Eval Value) -> (String, Value)
macro name f = (name, PrimFunc name $ \args -> wrapInvalidArgs f name args)

wrapInvalidArgs :: ([Value] -> Eval Value) -> String -> [Value] -> Eval Value
wrapInvalidArgs f name args = catchError (f args) $ \err -> case err of
    InvalidArguments -> throwError $ InvalidArguments' name args
    _                -> throwError err

-- Functions

logParserFun :: [Value] -> Eval Value
logParserFun [StringVal n, StringVal p] = logParser n p >> (return $ ListVal [])
logParserFun _ = throwError InvalidArguments

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> Eval Value
intBinOp op [IntVal x, IntVal y] = return $ IntVal (op x y)
intBinOp _ _                     = throwError InvalidArguments

genVar :: [Value] -> Eval Value
genVar [StringVal name] = genIdent name >>= return . Variable
genVar _                = throwError InvalidArguments

parseFun :: [Value] -> Eval Value
parseFun [StringVal s, ListVal l] = do
    tuples <- mapM getTuples l
    parsings <- parse s tuples
    return $ ListVal parsings
    where getTuples (FuncCall x [y]) = return (x, y)
          getTuples _                = throwError InvalidArguments
parseFun _ = throwError InvalidArguments

doFun :: [Value] -> Eval Value
doFun args@(_:_) = return $ last args
doFun _          = throwError InvalidArguments

cons :: [Value] -> Eval Value
cons [v, ListVal l] = return $ ListVal (v:l)
cons _              = throwError InvalidArguments

cat :: [Value] -> Eval Value
cat [StringVal x, StringVal y] = return $ StringVal $ x ++ y
cat _                          = throwError InvalidArguments

list :: [Value] -> Eval Value
list args = return $ ListVal args

evalFun :: [Value] -> Eval Value
evalFun [v] = eval v
evalFun _   = throwError InvalidArguments

startsWith :: [Value] -> Eval Value
startsWith [StringVal s, StringVal m] = return $ if isPrefixOf m s
    then StringVal m
    else ListVal []
startsWith _ = throwError InvalidArguments

dropFun :: [Value] -> Eval Value
dropFun [IntVal n, ListVal l] = return $ ListVal $ drop' n l where
    drop' 0 l'     = l'
    drop' k (_:xs) = drop' (k-1) xs
    drop' _ []     = []
dropFun [IntVal n, StringVal s] = do
    l <- strToList [StringVal s]
    l' <- dropFun [IntVal n, l]
    listToStr [l']
dropFun _ = throwError InvalidArguments

lengthFun :: [Value] -> Eval Value
lengthFun [ListVal l]   = return $ IntVal $ toInteger $ length l
lengthFun [StringVal s] = return $ IntVal $ toInteger $ length s
lengthFun _             = throwError InvalidArguments

strToList :: [Value] -> Eval Value
strToList [StringVal s] = return $ ListVal $ map (StringVal . (:[])) s
strToList _             = throwError InvalidArguments

listToStr :: [Value] -> Eval Value
listToStr v = listToStr' v >>= return . StringVal where
    listToStr' [ListVal ((StringVal s) : rest)] = do
        s' <- listToStr' [ListVal rest]
        return $ s ++ s'
    listToStr' [ListVal []] = return ""
    listToStr' _ = throwError InvalidArguments

-- Macros

set :: [Value] -> Eval Value
set [Variable n, v] = do
    v' <- eval v
    setVar n v'
    return $ ListVal []
set _ = throwError InvalidArguments

ifFun :: [Value] -> Eval Value
ifFun [cond, thenExpr, elseExpr] = do
    b <- eval cond
    case b of
        ListVal [] -> eval elseExpr
        _          -> eval thenExpr
ifFun _ = throwError InvalidArguments

apply :: [Value] -> Eval Value
apply (f:args) = do
    args' <- mapM eval args
    eval $ FuncCall f args'
apply _ = throwError InvalidArguments

quote :: [Value] -> Eval Value
quote [v] = return v
quote _   = throwError InvalidArguments
