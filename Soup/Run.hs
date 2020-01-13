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

import System.IO

import Debug.Trace

runEval :: (Env, DebugZipper) -> Eval a -> Either (InterpError, DebugTree) (a, DebugTree, Env)
runEval (initEnv, initDebug) x = case unwrapped of
    (Right x, (env, debugZip))  -> Right (x, rootTree debugZip, env)
    (Left err, (env, debugZip)) -> Left (err, rootTree debugZip)
    where
        unwrapped = runIdentity (runStateT (runExceptT (unwrapEval x))
                                           (initEnv, initDebug))

showDebugTree :: DebugTree -> String
showDebugTree t = showTree t where
    showTree t =
        concat .
        map showLine .
        groupTrees .
        map reverse .
        filter (not . null) .
        collectTree $ t

    collectTree t = collectTrees [] [stripEmpty t]
    collectTrees prev [Tree logs children] = collectTrees (logs ++ prev) (reverse children)
    collectTrees prev ts = prev : concat (map (\(Tree ls cs) -> collectTrees ls cs) ts)

    groupTrees = groupBy $ \a b -> startLineno a == startLineno b

    stripEmpty (Tree xs children) = Tree xs (map stripEmpty $ filter notEmpty children)
    notEmpty (Tree [] []) = False
    notEmpty _            = True

    showTrees line prev [Tree ((n, p) : rest) children]
        | lineno p > line =
            prev : showTrees (lineno p) (show (lineno p) ++ " " ++ n) [Tree rest children]
        | otherwise = showLog (lineno p) (prev ++ " " ++ n) rest
    showTrees line prev [Tree [] children] = showTrees line prev children
    showTrees line prev (t : ts)
        | startLineno logs > line = prev : showAll newPrev (t : ts)
        | otherwise = showTrees line prev [t] ++ showAll blankPrev ts
        where
            newPrev = show (startLineno t) ++ " "
            blankPrev = show line ++ replicate (length prev - length (show line)) ' '
            showAll customPrev trees =
                concat $ map (\x -> showTrees (startLineno t) customPrev [x]) trees

    startLineno (Tree ((n, p) : rest) _) = lineno p
    lineno p = nlines (program t) - nlines p + 1
    nlines = length . lines
    program (Tree [(_, p)] _) = p

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
    (exprs, tree, env) <- parseStr' s
    (vals, _, _) <- runEval (env, emptyTree) $ mapM eval exprs
    return $ last vals

debugFile :: String -> IO ()
debugFile fname = do
    file <- readFile fname
    case parseStr' file of
        Right (vals, tree, env) -> do
            print (ListVal vals)
            showTree tree
        Left (err, tree) -> do
            putStrLn $ "Error: " ++ show err
            showTree tree
    where
        showTree tree = do
            putStrLn "=== DEBUG OUTPUT ==="
            putStrLn $ showDebugTree tree
