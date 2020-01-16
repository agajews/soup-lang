{-# LANGUAGE FlexibleContexts #-}

module Soup.Builtins (
    builtins,
    function',
    macro'
) where

import Soup.Debugger
import Soup.Env
import Soup.Eval
import Soup.Parse
import Soup.Value

import Text.Read

import Control.Monad.Except

import Data.Char
import Data.List

builtins :: [(String, Value)]
builtins = [function "+" $ intBinOp (+),
            function "-" $ intBinOp (-),
            function "*" $ intBinOp (*),
            function ">" $ intBoolBinOp (>),
            function "<" $ intBoolBinOp (<),
            function ">=" $ intBoolBinOp (>=),
            function "<=" $ intBoolBinOp (<=),
            function "=" eqFun,
            function "!=" neqFun,
            function "read-int" readInt,
            function "show" showFun,
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
            function "or" orFun,
            function "and" andFun,
            function "all" allFun,
            function "any" anyFun,
            function "map" mapFun,
            function "is-alpha-num" alphaNumFun,
            function "is-digit" digitFun,
            function "." funcCallFun,
            macro "parse" parseMacro,
            macro "set!" set,
            macro "def!" def,
            macro "if" ifFun,
            macro "cond" condFun,
            macro "apply" apply,
            macro "quote" quote]

function :: String -> ([Value] -> Eval Value) -> (String, Value)
function name f = (name, PrimFunc name func) where
    func args = mapM eval args >>= wrapInvalidArgs f name

function' :: String -> ([Value] -> Eval Value) -> Value
function' name f = snd $ function name f

macro :: String -> ([Value] -> Eval Value) -> (String, Value)
macro name f = (name, PrimFunc name $ \args -> wrapInvalidArgs f name args)

macro' :: String -> ([Value] -> Eval Value) -> Value
macro' name f = snd $ macro name f

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

boolToVal :: Bool -> Value
boolToVal True  = IntVal 1
boolToVal False = ListVal []

intBoolBinOp :: (Integer -> Integer -> Bool) -> [Value] -> Eval Value
intBoolBinOp op [IntVal x, IntVal y] = case (op x y) of
    True  -> return $ IntVal x
    False -> return $ ListVal []
intBoolBinOp _ _ = throwError InvalidArguments

eqFun :: [Value] -> Eval Value
eqFun [IntVal x, IntVal y]       = return $ boolToVal (x == y)
eqFun [StringVal x, StringVal y] = return $ boolToVal (x == y)
eqFun _                          = return $ boolToVal False

neqFun :: [Value] -> Eval Value
eqFun [IntVal x, IntVal y]       = return $ boolToVal (x != y)
eqFun [StringVal x, StringVal y] = return $ boolToVal (x != y)
eqFun _                          = return $ boolToVal True

readInt :: [Value] -> Eval Value
readInt [StringVal s] = case readMaybe of
    Just x  -> return $ IntVal x
    Nothing -> throwError InvalidArguments
readInt _ = throwError InvalidArguments

showFun :: [Value] -> Eval Value
showFun [x] = return $ StringVal $ show x
showFun _   = throwError InvalidArguments

genVar :: [Value] -> Eval Value
genVar [StringVal name] = genIdent name >>= return . Variable
genVar _                = throwError InvalidArguments

doFun :: [Value] -> Eval Value
doFun args@(_:_) = return $ last args
doFun _          = throwError InvalidArguments

cons :: [Value] -> Eval Value
cons [v, ListVal l] = return $ ListVal (v:l)
cons _              = throwError InvalidArguments

cat :: [Value] -> Eval Value
cat args = do
    strings <- cat' args
    return $ StringVal $ concat strings
    where
        cat' (StringVal s : xs) = do
            rest <- cat' xs
            return (s : rest)
        cat' [] = return []
        cat' _ = throwError InvalidArguments

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
emptyStr [StringVal ""] = return $ StringVal ""
emptyStr [StringVal _]  = return $ ListVal []
emptyStr _              = throwError InvalidArguments

list :: [Value] -> Eval Value
list args = return $ ListVal args

listHead :: [Value] -> Eval Value
listHead [ListVal (x:_)] = return x
listHead _               = throwError InvalidArguments

listTail :: [Value] -> Eval Value
listTail [ListVal (_:xs)] = return $ ListVal xs
listTail _                = throwError InvalidArguments

valToScope :: Value -> Eval [Integer]
valToScope (ListVal l) = valToScope' l
    where
    valToScope' (IntVal x : xs) = do
        rest <- valToScope' xs
        return (x : rest)
    valToScope' [] = return []
    valToScope' _ = throwError InvalidArguments
valToScope _ = throwError InvalidArguments

evalFun :: [Value] -> Eval Value
evalFun [scope, v] = do
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
elemFun _ = throwError InvalidArguments

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

orFun :: [Value] -> Eval Value
orFun [] = return $ ListVal []
orFun (x:xs) = case x of
    ListVal [] -> orFun xs
    _          -> return x

andFun :: [Value] -> Eval Value
andFun [] = return $ ListVal []
andFun [x] = return x
andFun (x:xs) = case x of
    ListVal [] -> return $ ListVal []
    _          -> andFun xs

allFun :: [Value] -> Eval Value
allFun [ListVal l] = andFun l
allFun _           = throwError InvalidArguments

anyFun :: [Value] -> Eval Value
anyFun [ListVal l] = orFun l
anyFun _           = throwError InvalidArguments

mapFun :: [Value] -> Eval Value
mapFun [f, ListVal l] = do
    l' <- mapM (\x -> eval $ FuncCall f [x]) l
    return $ ListVal l'
mapFun _              = throwError InvalidArguments

alphaNumFun :: [Value] -> Eval Value
alphaNumFun [StringVal [c]] = case isAlphaNum c of
    True  -> return $ StringVal [c]
    False -> return $ ListVal []
alphaNumFun _ = throwError InvalidArguments

digitFun :: [Value] -> Eval Value
digitFun [StringVal [c]] = case isDigit c of
    True  -> return $ StringVal [c]
    False -> return $ ListVal []
digitFun _ = throwError InvalidArguments

funcCallFun :: [Value] -> Eval Value
funcCallFun (f : args) = return $ FuncCall f args
funcCallFun _          = throwError InvalidArguments

-- Macros

set :: [Value] -> Eval Value
set [Variable n, v] = do
    v' <- eval v
    setVar n v'
    return $ ListVal []
set _ = throwError InvalidArguments

def :: [Value] -> Eval Value
def [scope, Variable n, v] = do
    v' <- eval v
    scope' <- eval scope >>= valToScope
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

condMacro :: [Value] -> Eval Value
condMacro (FuncCall cond [action] : rest) = do
    b <- eval cond
    case b of
        ListVal [] -> condMacro rest
        _          -> eval action
condMacro [] = return $ ListVal []
condMacro _ = throwError InvalidArguments

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

parseMacro :: [Value] -> Eval Value
parseMacro (s : pairs) = do
    s' <- eval s
    case s' of
        StringVal s'' -> do
            pairs' <- mapM getPairs pairs
            parsings <- parse s'' pairs'
            return $ ListVal parsings
        _ -> throwError InvalidArguments
    where
        getPairs (FuncCall x [y]) = do
            x' <- eval x
            y' <- eval y
            return (x', y')
        getPairs _                = throwError InvalidArguments
parseMacro _ = throwError InvalidArguments
