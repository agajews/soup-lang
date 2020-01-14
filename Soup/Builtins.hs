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

import Debug.Trace

builtins :: [(String, Value)]
builtins = [function "+" $ intBinOp (+),
            function "-" $ intBinOp (-),
            function "*" $ intBinOp (*),
            function "parse" parseFun,
            function "gen-var" genVar,
            function "do" doFun,
            function "cons" cons,
            function "head" listHead,
            function "tail" listTail,
            function "list" list,
            function "eval" evalFun,
            function "starts-with" startsWith,
            function "length" lengthFun,
            function "elem" elemFun,
            function "cat" cat,
            function "span" spanFun,
            function "empty-str" emptyStr,
            function "drop" dropFun,
            function "str->list" strToList,
            function "list->str" listToStr,
            function "log-parser" logParserFun,
            function "print" printFun,
            function "puts" putsFun,
            function "scope" scopeFun,
            macro "set!" set,
            macro "def!" def,
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

spanFun :: [Value] -> Eval Value
spanFun [predicate, StringVal s] = do
    (match, rest) <- span' s
    return $ ListVal [StringVal match, StringVal rest]
    where
        span' (x:xs) = do
            b <- eval $ FuncCall predicate [StringVal [x]]
            case b of
                ListVal [] -> return ("", x:xs)
                _ -> do
                    (match, rest) <- span' xs
                    return (x:match, rest)
        span' "" = return ("", "")
spanFun _ = throwError InvalidArguments

emptyStr :: [Value] -> Eval Value
emptyStr [StringVal ""] = return $ ListVal []
emptyStr [StringVal s]  = return $ StringVal s
emptyStr _              = throwError InvalidArguments

list :: [Value] -> Eval Value
list args = return $ ListVal args

listHead :: [Value] -> Eval Value
listHead [ListVal (x:_)] = return x
listHead _               = throwError InvalidArguments

listTail :: [Value] -> Eval Value
listTail [ListVal (_:xs)] = return $ ListVal xs
listTail _                = throwError InvalidArguments

valToScope :: [Value] -> Eval [Integer]
valToScope (IntVal x : xs) = do
    rest <- valToScope xs
    return (x : rest)
valToScope [] = return []
valToScope _ = throwError InvalidArguments

evalFun :: [Value] -> Eval Value
evalFun [ListVal scope, v] = do
    callerScope <- getScope
    scope' <- valToScope scope
    setScope scope'
    res <- eval v
    setScope callerScope
    return res
evalFun _          = throwError InvalidArguments

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

elemFun :: [Value] -> Eval Value
elemFun [ListVal l, x] = return $ case x `elem` l of
    True  -> x
    False -> ListVal []

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

def :: [Value] -> Eval Value
def [ListVal scope, Variable n, v] = do
    v' <- eval v
    scope' <- valToScope scope
    defVar n v' scope'
    return $ ListVal []
def _ = throwError InvalidArguments

ifFun :: [Value] -> Eval Value
ifFun [cond, thenExpr, elseExpr] = do
    b <- eval cond
    case b of
        ListVal [] -> eval elseExpr
        _          -> eval thenExpr
ifFun _ = throwError InvalidArguments

apply :: [Value] -> Eval Value
apply (f:args) = do
    -- prescope <- getScope
    -- traceShow ("pre-apply scope", prescope) $ return ()
    args' <- mapM eval args
    res <- eval $ FuncCall f args'
    -- postscope <- getScope
    -- traceShow ("post-apply scope", postscope) $ return ()
    return res
apply _ = throwError InvalidArguments

quote :: [Value] -> Eval Value
quote [v] = return v
quote _   = throwError InvalidArguments

printFun :: [Value] -> Eval Value
printFun [v] = liftIO (print v) >> return v
printFun _   = throwError InvalidArguments

putsFun :: [Value] -> Eval Value
putsFun [StringVal s] = liftIO (putStrLn s) >> return (StringVal s)
putsFun _             = throwError InvalidArguments

scopeFun :: [Value] -> Eval Value
scopeFun [] = do
    s <- getScope
    return $ scopeToVal s
scopeFun _ = throwError InvalidArguments
