module Soup.Run (
    parseStr,
    runStr,
) where

import Soup.Bootstrap
import Soup.Debugger
import Soup.Env
import Soup.Eval
import Soup.Parse
import Soup.Parser
import Soup.Value

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import System.IO

runEval :: (Env, DebugZipper) -> Eval a -> Either (InterpError, DebugTree) (a, Env)
runEval (initEnv, initDebug) x = case unwrapped of
    (Right x, (env, debugZip))  -> Right (x, env)
    (Left err, (env, debugZip)) -> Left (err, rootTree debugZip)
    where
        unwrapped = runIdentity (runStateT (runExceptT (unwrapEval x))
                                           (initEnv, initDebug))

showDebugTree t@(Tree (_, program) _) = showEntry 0 t where
    showEntry k (Tree (n, p) children) =
        indent k ++ n ++ " (" ++ (show $ lineno p) ++ ")\n" ++
        (concat $ map (showEntry $ k + 1) children)
    indent k = concat $ replicate k "| "
    lineno p = nlines program - nlines p + 1
    nlines = length . lines

parseStr' :: String -> Either (InterpError, DebugTree) ([Value], Env)
parseStr' s = runEval (emptyEnv, emptyTree ("parse-root", s)) $ do
    t <- initType
    parsing <- parse s [(t, contToVal "final-continuation" finalContinuation)]
    case parsing of
        [ListVal l] -> return l
        []          -> throwError ParsingError
        _           -> undefined

finalContinuation :: Value -> String -> Eval [Value]
finalContinuation v s = return $ case s of
    "" -> [v]
    _  -> []

parseStr :: String -> Either (InterpError, DebugTree) [Value]
parseStr = liftM fst . parseStr'

runStr :: String -> Either (InterpError, DebugTree) Value
runStr s = do
    (exprs, env) <- parseStr' s
    (vals, _) <- runEval (env, emptyTree ("run-root", "")) $ mapM eval exprs
    return $ last vals

parseFile :: String -> IO ()
parseFile fname = do
    file <- readFile fname
    case parseStr file of
        Right vals       -> print vals
        Left (err, tree) -> do
            putStrLn $ "Error: " ++ show err
            putStrLn "=== DEBUG OUTPUT ==="
            putStrLn $ showDebugTree tree
