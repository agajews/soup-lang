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
    parsings <- runEval initEnv $ parse s initType
    case filter (null . snd) parsings of
        [(v, _)] -> runEval' initEnv v
        [] -> Left ParsingError
        _ -> Left AmbiguousParsing

parseStr :: String -> Either InterpError Value
parseStr = liftM fst . parseStr'
    
runStr :: String -> Either InterpError Value
runStr s = do
    (expr, env) <- parseStr' s
    runEval env $ eval expr

