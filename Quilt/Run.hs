module Quilt.Run (
    parseStr,
    runStr,
) where

import Quilt.Value
import Quilt.Env
import Quilt.Eval
import Quilt.Parse
import Quilt.Bootstrap

import Control.Monad.Except

parseStr' :: String -> Either InterpError (Value, Env)
parseStr' s = do
    parsing <- runEval emptyEnv $ initType >>= parse (PrimFunc emptyRule) s
    case parsing of
        ([v], env) -> return (v, env)
        ([], _) -> throwError ParsingError

emptyRule :: [Value] -> Eval Value
emptyRule [v, StringVal s] = return $ ListVal [v, StringVal s]
emptyRule _ = throwError InvalidArguments

parseStr :: String -> Either InterpError Value
parseStr = liftM fst . parseStr'
    
runStr :: String -> Either InterpError Value
runStr s = do
    (expr, env) <- parseStr' s
    (v, _) <- runEval env $ eval expr
    return v
