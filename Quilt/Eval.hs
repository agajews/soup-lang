module Quilt.Eval (
    InterpError(..),
    Eval,
    runEval,
    runEval',
    eval,
) where

import Quilt.Value
import Quilt.Env

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

runEval' :: Env -> Eval a -> Either InterpError (a, Env)
runEval' env x = runIdentity (runExceptT (runStateT (unwrapEval x) env))

runEval :: Env -> Eval a -> Either InterpError a
runEval env x = liftM fst $ runIdentity (runExceptT (runStateT (unwrapEval x) env))

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
