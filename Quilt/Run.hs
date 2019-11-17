module Quilt.Run (
    parseStr,
    runStr,
) where

import Quilt.Value
import Quilt.Env
import Quilt.Eval
import Quilt.Parse
import Quilt.Bootstrap

import Control.Monad

parseStr' :: String -> Either InterpError (Value, Env)
parseStr' s = do
    parsings <- runEval emptyEnv $ initType >>= parse s
    case filter (null . snd) parsings of
        [(v, env, _)] -> Right (v, env)
        [] -> Left ParsingError
        _ -> Left AmbiguousParsing

parseStr :: String -> Either InterpError Value
parseStr = liftM fst . parseStr'
    
runStr :: String -> Either InterpError Value
runStr s = do
    (expr, env) <- parseStr' s
    runEval env $ eval expr

