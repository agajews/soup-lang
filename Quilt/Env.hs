module Quilt.Env (
    Env,
    emptyEnv,
    genVar,
    getVar,
    setVar,
) where

import Quilt.Value

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

emptyEnv :: Env
emptyEnv = Env 0 Map.empty

genIdent :: Eval Ident
genIdent = do
    Env n env <- get
    let n' = n + 1
    put $ Env n' env
    return $ Ident n'

getVar :: Ident -> Eval Value
getVar n = do
    Env _ env <- get
    case Map.lookup n env of
        Just v -> return v
        Nothing -> throwError (UnboundVariable n)

setVar :: Ident -> Value -> Eval ()
setVar n v = do
    Env m env <- get
    put $ Env m (Map.insert n v env)

modifyVar :: Ident -> (Value -> Value) -> Eval ()
modifyVar n f = do
    v <- getVar n
    setVar n (f v)

deleteVar :: Value -> Eval ()
deleteVar (Variable n) = do
    Env _ env <- get
    put $ Env m (Map.delete n env)
