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
eval x@(Lambda _ _ _) = return x
eval (Variable n) = getVar n
eval (FuncCall f args) = do
    f' <- eval f
    case f' of
        PrimFunc _ f'' -> f'' args
        Lambda params defScope body -> do
            callerScope <- getScope
            -- liftIO $ print $ ("caller scope", f, callerScope)
            pushScope defScope
            let args' = (scopeToVal callerScope) : args
            catchError (matchArgs params args') $ \err -> case err of
                MismatchedArgs -> throwError $ MismatchedArgs' params args'
                _              -> throwError err
            res <- eval body
            setScope callerScope
            -- liftIO $ print $ ("returning to scope", f, callerScope)
            return res
        _ -> throwError $ NotAFunction f'

matchArgs :: [Ident] -> [Value] -> Eval ()
matchArgs (n:ns) (v:vs) = setVar n v >> matchArgs ns vs
matchArgs [] []         = return ()
matchArgs _ _           = throwError MismatchedArgs
