{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map


data InterpError = UnboundVariable Ident
                 | TooFewArgs
                 | TooManyArgs
    deriving (Show)

type Env = (Map.Map Ident Value)

newtype Eval a = Eval { unwrapEval :: ReaderT Env (ExceptT InterpError Identity) a }
    deriving (Functor,
              Applicative,
              Monad,
              MonadError InterpError,
              MonadReader Env)

runEval :: Env -> Eval a -> Either InterpError a
runEval env x = runIdentity (runExceptT (runReaderT (unwrapEval x) env))

newtype Ident = Ident Integer
    deriving (Show, Eq, Ord)

data Lambda = Lambda { params :: [Ident],
                       env :: Env,
                       body :: Value }

data Value = StringVal String
           | IntVal Integer
           | ListVal [Value]
           | PrimFunc ([Value] -> Eval Value)
           | LambdaVal Lambda
           | Variable Ident
           | FuncCall Value [Value]

eval :: Value -> Eval Value
eval x@(StringVal _) = return x
eval x@(IntVal _) = return x
eval x@(ListVal _) = return x
eval x@(PrimFunc _) = return x
eval x@(LambdaVal _) = return x
eval (Variable n) = do
    env <- ask
    case Map.lookup n env of
        Just x -> return x
        Nothing -> throwError $ UnboundVariable n
eval (FuncCall fExp argExps) = do
    fVal <- eval fExp
    args <- mapM eval argExps
    case fVal of
        PrimFunc f -> f args
        LambdaVal l -> applyLambda l args

applyLambda :: Lambda -> [Value] -> Eval Value
applyLambda (Lambda { params, env, body }) args =
    local (const env) (matchArgs params args >> eval body)

matchArgs :: [Ident] -> [Value] -> Eval ()
matchArgs (n:ns) (v:vs) = local (\env -> (Map.insert n v env)) (matchArgs ns vs)
matchArgs [] [] = return ()
matchArgs _ [] = throwError TooFewArgs
matchArgs [] _ = throwError TooManyArgs
