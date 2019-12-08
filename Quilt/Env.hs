module Quilt.Env (
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

import Quilt.Value

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

emptyEnv :: Env
emptyEnv = Env 0 Map.empty

genIdent :: String -> Eval Ident
genIdent name = do
    Env n env <- getEnv
    let n' = n + 1
    putEnv $ Env n' env
    return $ Ident name n'

getVar :: Ident -> Eval Value
getVar n = do
    Env _ env <- getEnv
    case Map.lookup n env of
        Just v -> return v
        Nothing -> throwError (UnboundVariable n)

setVar :: Ident -> Value -> Eval ()
setVar n v = do
    Env m env <- getEnv
    putEnv $ Env m (Map.insert n v env)

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
