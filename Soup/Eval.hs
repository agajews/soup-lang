module Soup.Eval (
    InterpError(..),
    eval,
) where

import Soup.Env
import Soup.Value

import Control.Monad.Except

eval :: Value -> Eval Value
eval x@(StringVal _) = return x
eval x@(IntVal _) = return x
eval x@(ListVal _) = return x
eval x@(PrimFunc _ _) = return x
eval x@(Lambda _ _) = return x
eval (Variable n) = getVar n
eval (FuncCall f args) = do
    f' <- eval f
    case f' of
        PrimFunc _ f'' -> f'' args
        Lambda params body -> do
            catchError (matchArgs params args) $ \err -> case err of
                MismatchedArgs -> throwError $ MismatchedArgs' params args
                _              -> throwError err
            res <- eval body
            return res
        _ -> throwError $ NotAFunction f'

matchArgs :: [Ident] -> [Value] -> Eval ()
matchArgs (n:ns) (v:vs) = setVar n v >> matchArgs ns vs
matchArgs [] []         = return ()
matchArgs _ []          = throwError MismatchedArgs
matchArgs [] _          = throwError MismatchedArgs
