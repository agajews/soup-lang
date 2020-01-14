module Soup.Env (
    Env,
    emptyEnv,
    genIdent,
    getVar,
    modifyVar,
    setVar,
    deleteVar,
    getEnv,
    putEnv,
) where

import Soup.Value

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

emptyEnv :: Env
emptyEnv = Env 0 [] (EnvMap Map.empty)

genIdent :: String -> Eval Ident
genIdent name = do
    Env n scope env <- getEnv
    let n' = n + 1
    putEnv $ Env n' scope env
    return $ Ident name n'

pushScope :: Eval ()
pushScope = do
    Env n scope env <- getEnv
    let n' = n + 1
    putEnv $ Env n' (n' : scope) (Map.insert n' Map.empty env)

getEnvs :: Eval [EnvMap]
getEnvs = do
    Env _ scope env <- getEnv
    return $ getEnvs' scope
    where
        getEnvs' (x:xs) = (next Map.! x) : rest where
            (next : rest) = getEnvs' xs
        getEnvs' [] = [env]

getVar :: Ident -> Eval Value
getVar n = do
    envs <- getEnvs
    getVar' envs
    where
        getVar' (e:es) = case Map.lookup n e of
            Just v  -> return v
            Nothing -> getVar' es
        getVar' [] = throwError (UnboundVariable n)

setVar :: Ident -> Value -> Eval ()
setVar n v = do
    Env m scope globalEnv <- getEnv
    let scope' = reverse scope
    case setVar' scope' globalEnv of
        Just globalEnv' -> putEnv Env m scope globalEnv'
        Nothing         -> putEnv Env m scope (defVar scope' globalEnv)

    where
        setVar' (x:xs) env = case setVar' xs (env Map.! x) of
            Just env' -> Just $ Map.insert x env' env
            Nothing   -> setVar' [] env
        setVar' [] env = case Map.lookup n env of
            Just _  -> Just $ Map.insert n v env
            Nothing -> Nothing

        defVar (x:xs) env = defVar xs (env Map.! x)
        defVar [] env     = Map.insert n v env

modifyVar :: Ident -> (Value -> Eval Value) -> Eval ()
modifyVar n f = do
    v <- getVar n
    v' <- f v
    setVar n v'

deleteVar :: Ident -> Eval ()
deleteVar n = do
    Env m env <- getEnv
    putEnv $ Env m (Map.delete n env)

getEnv :: Eval Env
getEnv = get >>= return . fst

putEnv :: Env -> Eval ()
putEnv env = do
    (_, debugTree) <- get
    put (env, debugTree)
