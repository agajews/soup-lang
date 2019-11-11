{-# LANGUAGE FlexibleContexts #-}

module Quilt.Env (
    Env,
    emptyEnv,
    genIdent,
    getVar,
    setVar,
) where

import Quilt.Value

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

emptyEnv :: Env
emptyEnv = Env 0 Map.empty

genIdent :: (MonadState Env m) => m Ident
genIdent = do
    Env n env <- get
    let n' = n + 1
    put $ Env n' env
    return $ Ident n'

getVar :: (MonadState Env m, MonadError InterpError m) => Ident -> m Value
getVar n = do
    Env _ env <- get
    case Map.lookup n env of
        Just v -> return v
        Nothing -> throwError (UnboundVariable n)

setVar :: (MonadState Env m) => Ident -> Value -> m ()
setVar n v = do
    Env m env <- get
    put $ Env m (Map.insert n v env)
    return ()

