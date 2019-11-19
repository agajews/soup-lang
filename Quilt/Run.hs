module Quilt.Run (
    parseStr,
    runStr,
) where

import Quilt.Value
import Quilt.Env
import Quilt.Eval
import Quilt.Parse
import Quilt.Parser
import Quilt.Bootstrap

import Control.Monad.Except

parseStr' :: String -> Either InterpError (Value, Env)
parseStr' s = do
    parsing <- runEval emptyEnv $ do
        t <- initType
        parse s [(t, contToVal finalContinuation)]
    case parsing of
        ([v], env) -> return (v, env)
        ([], _) -> throwError ParsingError
        _ -> undefined

finalContinuation :: Value -> String -> Eval [Value]
finalContinuation v s = return $ case s of
    "" -> [v]
    _ -> []

parseStr :: String -> Either InterpError Value
parseStr = liftM fst . parseStr'
    
runStr :: String -> Either InterpError Value
runStr s = do
    (expr, env) <- parseStr' s
    (v, _) <- runEval env $ eval expr
    return v
