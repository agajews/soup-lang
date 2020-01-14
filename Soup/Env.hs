module Soup.Env (
    Env,
    emptyEnv,
    genIdent,
    getVar,
    modifyVar,
    setVar,
    getEnv,
    putEnv,
    pushScope,
    getScope,
    setScope,
) where

import Soup.Value

import Control.Monad.Except
import Control.Monad.State

import Data.List

import qualified Data.Map as Map

import Debug.Trace

type EnvMap' = Map.Map Ident (Either EnvMap Value)

emptyEnv :: Env
emptyEnv = Env 0 [] (EnvMap Map.empty)

genIdent :: String -> Eval Ident
genIdent name = do
    Env n scope env <- getEnv
    let n' = n + 1
    putEnv $ Env n' scope env
    return $ Ident name n'

extractEnv :: EnvMap' -> Integer -> EnvMap'
extractEnv env x = case Map.lookup (Ident "#scope" x) env of
    Just (Left (EnvMap env')) -> env'
    _                         -> traceShow ("failed with env", x, env) undefined

insertEnv :: EnvMap' -> Integer -> EnvMap' -> EnvMap'
insertEnv env' x env = Map.insert (Ident "#scope" x) (Left $ EnvMap env') env

pushScope :: [Integer] -> Eval ()
pushScope context = do
    -- trace "\n" $ return ()
    -- traceShow ("pushing scope", context) $ return ()
    Env n _ (EnvMap globalEnv) <- getEnv
    let n' = n + 1
    let env' = pushScope' (reverse context) n' globalEnv
    -- traceShow ("created env", env') $ return ()
    putEnv $ Env n' (n' : context) (EnvMap env')
    where
        pushScope' (x:xs) n' env = insertEnv (pushScope' xs n' (extractEnv env x)) x env
        pushScope' [] n' env     = insertEnv Map.empty n' env

getScope :: Eval [Integer]
getScope = do
    Env _ scope _ <- getEnv
    return scope

setScope :: [Integer] -> Eval ()
setScope newScope = do
    Env n _ env <- getEnv
    putEnv $ Env n newScope env

getVar :: Ident -> Eval Value
getVar n = do
    -- trace "\n" $ return ()
    -- traceShow ("getting", n) $ return ()
    Env _ scope (EnvMap globalEnv) <- getEnv
    -- traceShow scope $ return ()
    -- trace (intercalate "\n" $ map show $ Map.toList globalEnv) $ return ()
    case getVar' (reverse scope) globalEnv of
        Just (Right v) -> return v
        _              -> throwError (UnboundVariable n)
    where
        getVar' (x:xs) env = case getVar' xs (extractEnv env x) of
            Just v  -> Just v
            Nothing -> getVar' [] env
        getVar' [] env = Map.lookup n env

setVar :: Ident -> Value -> Eval ()
setVar n v = do
    -- trace "\n" $ return ()
    -- traceShow ("setting", n, v) $ return ()
    Env m scope (EnvMap globalEnv) <- getEnv
    let scope' = reverse scope
    -- traceShow scope $ return ()
    -- trace (intercalate "\n" $ map show $ Map.toList globalEnv) $ return ()
    case setVar' scope' globalEnv of
        Just globalEnv' -> putEnv $ Env m scope (EnvMap globalEnv')
        Nothing         -> putEnv $ Env m scope (EnvMap $ defVar scope' globalEnv)
    where
        setVar' (x:xs) env = case setVar' xs (extractEnv env x) of
            Just env' -> Just $ insertEnv env' x env
            Nothing   -> setVar' [] env
        setVar' [] env = case Map.lookup n env of
            Just _  -> Just $ Map.insert n (Right v) env
            Nothing -> Nothing

        defVar (x:xs) env = insertEnv (defVar xs $ extractEnv env x) x env
        defVar [] env     = Map.insert n (Right v) env

modifyVar :: Ident -> (Value -> Eval Value) -> Eval ()
modifyVar n f = do
    v <- getVar n
    v' <- f v
    setVar n v'

getEnv :: Eval Env
getEnv = get >>= return . fst

putEnv :: Env -> Eval ()
putEnv env = do
    (_, debugTree) <- get
    put (env, debugTree)
