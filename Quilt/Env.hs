module Quilt.Env (
    Env,
    emptyEnv,
    genIdent,
    getVar,
    modifyVar,
    setVar,
    deleteVar,
) where

import Quilt.Value

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

emptyEnv :: Env
emptyEnv = Env 0 Map.empty

genIdent :: String -> Eval Ident
genIdent name = do
    Env n env <- get
    let n' = n + 1
    put $ Env n' env
    return $ Ident name n'

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

modifyVar :: Ident -> (Value -> Eval Value) -> Eval ()
modifyVar n f = do
    v <- getVar n
    v' <- f v
    setVar n v'

deleteVar :: Value -> Eval ()
deleteVar (Variable n) = do
    Env m env <- get
    put $ Env m (Map.delete n env)
