module Soup.Run (
    parseStr,
    parseStr',
    runStr,
    debugFile,
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

import Data.List

import Debug.Trace

runEval :: (Env, DebugZipper) -> Eval a -> Either (InterpError, DebugTree) (a, DebugTree, Env)
runEval (initEnv, initDebug) x = case unwrapped of
    (Right y, (env, debugZip)) -> Right (y, rootTree debugZip, env)
    (Left err, (_, debugZip))  -> Left (err, rootTree debugZip)
    where
        unwrapped = runIdentity (runStateT (runExceptT (unwrapEval x))
                                           (initEnv, initDebug))

showDebugTree :: DebugTree -> String
showDebugTree tree = showTree tree where
    showTree t = intercalate "\n" (showTrees 0 "" [flipLogs $ stripEmpty t])

    stripEmpty (Tree xs children) = Tree xs (map stripEmpty $ filter notEmpty children)

    notEmpty (Tree [] []) = False
    notEmpty _            = True

    flipLogs (Tree logs children) = Tree (reverse logs) (map flipLogs children)

    -- justNames (Tree logs children) = Tree (map fst logs) (map justNames children)

    -- shallowNames (Tree logs _) = map fst logs

    showTrees line prev [Tree ((n, p) : rest) children]
        | lineno p > line =
            prev : showTrees (lineno p) (show (lineno p) ++ " " ++ n) [Tree rest children]
        | otherwise = showTrees (lineno p) (prev ++ " " ++ n) [Tree rest children]
    showTrees line prev [Tree [] children] = showTrees line prev children
    showTrees _ prev [] = [prev]
    showTrees line prev (t : ts)
        | startLineno t > line = prev : showAll newPrev (t : ts)
        | otherwise = showTrees line prev [t] ++ showAll blankPrev ts
        where
            newPrev = show (startLineno t) ++ " "
            blankPrev = show line ++ replicate (length prev - length (show line) - 1) ' '
            showAll customPrev trees =
                concat $ map (\x -> showTrees (startLineno t) customPrev [x]) trees

    startLineno (Tree ((_, p) : _) _) = lineno p
    startLineno _                     = undefined
    lineno p = nlines (program tree) - nlines p + 1
    nlines = length . lines
    program (Tree [(_, p)] _) = p
    program _                 = undefined

parseStr' :: String -> Either (InterpError, DebugTree) ([Value], DebugTree, Env)
parseStr' s = runEval (emptyEnv, emptyTree) $ do
    t <- initType
    logParser "ROOT" s
    parsing <- parse s [(t, contToVal "final-continuation" finalContinuation)]
    case parsing of
        [ListVal l] -> return l
        []          -> throwError ParsingError
        _           -> undefined

finalContinuation :: Value -> String -> Eval [Value]
finalContinuation v s = case s of
    "" -> logParser "SUCCESS" "" >> return [v]
    _  -> return []

parseStr :: String -> Either (InterpError, DebugTree) [Value]
parseStr s = do
    (val, _, _) <- parseStr' s
    return val

runStr :: String -> Either (InterpError, DebugTree) Value
runStr s = do
    (exprs, _, env) <- parseStr' s
    (vals, _, _) <- runEval (env, emptyTree) $ mapM eval exprs
    return $ last vals

debugFile :: String -> IO ()
debugFile fname = do
    file <- readFile fname
    case parseStr' file of
        Right (vals, tree, _) -> do
            print (ListVal vals)
            showTree tree
        Left (err, tree) -> do
            putStrLn $ "Error: " ++ show err
            showTree tree
    where
        showTree tree = do
            putStrLn "=== DEBUG OUTPUT ==="
            putStrLn $ showDebugTree tree
