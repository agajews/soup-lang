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
import System.IO

parseStr' :: String -> Either InterpError ([Value], Env)
parseStr' s = do
    parsing <- runEval emptyEnv $ do
        t <- initType
        parse s [(t, contToVal "final-continuation" finalContinuation)]
    case parsing of
        ([ListVal l], env) -> return (l, env)
        ([], _) -> throwError ParsingError
        _ -> undefined

finalContinuation :: Value -> String -> Eval [Value]
finalContinuation v s = return $ case s of
    "" -> [v]
    _ -> []

parseStr :: String -> Either InterpError [Value]
parseStr = liftM fst . parseStr'
    
runStr :: String -> Either InterpError Value
runStr s = do
    (exprs, env) <- parseStr' s
    (vals, _) <- runEval env $ mapM eval exprs
    return $ last vals

parseFile :: String -> IO (Either InterpError [Value])
parseFile fname = readFile fname >>= return . parseStr
