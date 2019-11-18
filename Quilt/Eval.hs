module Quilt.Eval (
    InterpError(..),
    eval,
) where

import Quilt.Value
import Quilt.Env

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

eval :: Value -> Eval Value
eval x@(StringVal _) = return x
eval x@(IntVal _) = return x
eval x@(ListVal _) = return x
eval x@(PrimFunc _) = return x
eval x@(Lambda _ _) = return x
eval (Variable n) = getVar n
eval (FuncCall f args) = do
    f' <- eval f
    case f' of
        PrimFunc f' -> f' args
        Lambda params body -> do
            res <- matchArgs params args >> eval body
            mapM (deleteVar . Variable) params
            return res

matchArgs :: [Ident] -> [Value] -> Eval ()
matchArgs (n:ns) (v:vs) = setVar n v >> matchArgs ns vs
matchArgs [] [] = return ()
matchArgs _ [] = throwError TooFewArgs
matchArgs [] _ = throwError TooManyArgs
