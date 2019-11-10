{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map


data InterpError = UnboundVariable Ident
                 | TooFewArgs
                 | TooManyArgs
    deriving (Show)

data Env = Env Integer (Map.Map Ident Value)

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

newtype Eval a = Eval { unwrapEval :: StateT Env (ExceptT InterpError Identity) a }
    deriving (Functor,
              Applicative,
              Monad,
              MonadError InterpError,
              MonadState Env)

runEval :: Env -> Eval a -> Either InterpError (a, Env)
runEval env x = runIdentity (runExceptT (runStateT (unwrapEval x) env))

newtype Ident = Ident Integer
    deriving (Show, Eq, Ord)

data Value = StringVal String
           | IntVal Integer
           | ListVal [Value]
           | PrimFunc ([Value] -> Eval Value)
           | LambdaVal [Ident] Value
           | Variable Ident
           | FuncCall Value [Value]

eval :: Value -> Eval Value
eval x@(StringVal _) = return x
eval x@(IntVal _) = return x
eval x@(ListVal _) = return x
eval x@(PrimFunc _) = return x
eval x@(LambdaVal _ _) = return x
eval (Variable n) = getVar n
eval (FuncCall fExp argExps) = do
    fVal <- eval fExp
    args <- mapM eval argExps
    case fVal of
        PrimFunc f -> f args
        LambdaVal params body -> matchArgs params args >> eval body

matchArgs :: [Ident] -> [Value] -> Eval ()
matchArgs (n:ns) (v:vs) = setVar n v >> matchArgs ns vs
matchArgs [] [] = return ()
matchArgs _ [] = throwError TooFewArgs
matchArgs [] _ = throwError TooManyArgs
