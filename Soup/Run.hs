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

debugMaxlen :: Int
debugMaxlen = 100

debugMinlen :: Int
debugMinlen = 20

runEval :: (Env, DebugZipper) -> Eval a -> Either (InterpError, DebugTree) (a, DebugTree, Env)
runEval (initEnv, initDebug) x = case unwrapped of
    (Right x, (env, debugZip))  -> Right (x, rootTree debugZip, env)
    (Left err, (env, debugZip)) -> Left (err, rootTree debugZip)
    where
        unwrapped = runIdentity (runStateT (runExceptT (unwrapEval x))
                                           (initEnv, initDebug))

showDebugTree :: DebugTree -> String
showDebugTree t = showTree t where
    showTree t = concat (map showLine $ collectTree t)
    collectTree t = collectTrees 0 [] [stripEmpty t]

    collectTrees k prev [Tree logs children] =
        collectTrees k (logs ++ prev) (reverse children)
    collectTrees k prev ts = (k, prev) : concat (map (fullEntry $ k + 1) ts)

    fullEntry k (Tree logs children) = collectTrees k logs children

    stripEmpty (Tree xs children) = Tree xs (map stripEmpty $ filter notEmpty children)

    notEmpty (Tree [] []) = False
    notEmpty _            = True

    showLine (k, logs) = intercalate "\n" (firstLine : map wrapLine (tail logNames)) ++ "\n"
        where
            firstLine = indent k ++ concat (head logNames)
            wrapLine ws = emptyIndent k ++ concat ws
            logNames = wrapSplit lineLen . map (++ " ") . reverse . map fst $ logs
            lineLen = max (debugMaxlen - (length $ indent k)) debugMinlen

    wrapSplit l words = wrapSplit' words [] 0 where
        wrapSplit' (w:ws) curr 0 = wrapSplit' ws (w:curr) (length w)
        wrapSplit' (w:ws) curr k
            | l - k >= length w = wrapSplit' ws (w:curr) (k + length w)
            | otherwise = reverse curr : wrapSplit' (w:ws) [] 0
        wrapSplit' [] curr _ = [reverse curr]

    indent k = concat $ replicate k "| "
    emptyIndent k = replicate (2 * k) ' '
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
