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
import Control.Monad.State

import Data.Function
import Data.List

import qualified Data.Map as Map

import Text.Printf

runEval :: (Env, DebugZipper) -> Eval a ->
    IO (Either (InterpError, DebugTree, Env) (a, DebugTree, Env))
runEval (initEnv, initDebug) x = do
    contents <- unwrapped
    return $ case contents of
        (Right y, (env, debugZip))  -> Right (y, rootTree debugZip, env)
        (Left err, (env, debugZip)) -> Left (err, rootTree debugZip, env)
    where
        unwrapped = runStateT (runExceptT (unwrapEval x)) (initEnv, initDebug)

showDebugTree :: DebugTree -> String
showDebugTree tree = showTree tree where
    processTree t = sortMarked . markDepth 0 . flipLogs . stripEmpty $ t
    showTree t = intercalate "\n" (showTrees 0 "" [processTree t])
    flipLogs (Tree logs children) = Tree (reverse logs) (map flipLogs children)
    stripEmpty (Tree xs children) = Tree xs (map stripEmpty $ filter notEmpty children)

    notEmpty (Tree [] []) = False
    notEmpty _            = True

    markedToDepth (Tree (d, _) _) = d

    markDepth d (Tree logs []) = Tree (d, logs) []
    markDepth d (Tree logs children) = Tree (m, logs) markedChildren where
        markedChildren = map (markDepth (d + 1 :: Int)) children
        m = maximum (map markedToDepth markedChildren)

    sortMarked (Tree (_, logs) children) =
        Tree logs (map sortMarked $ sortBy (compare `on` markedToDepth) children)

    showTrees line prev [Tree ((n, p) : rest) children]
        | lineno p > line =
            prev : showTrees (lineno p) (showLineno (lineno p) 1 ++ " " ++ n) [Tree rest children]
        | otherwise = showTrees (lineno p) (prev ++ " " ++ n) [Tree rest children]
    showTrees line prev [Tree [] children] = showTrees line prev children
    showTrees _ prev [] = [prev]
    showTrees line prev (t : ts)
        | startLineno t > line = prev : showTrees line newPrev [t] ++ showAll barPrev
        | otherwise = showTrees line prev [t] ++ showAll blankPrev
        where
            newPrev = showLineno (startLineno t) 1
            barPrev = "|" ++ showLineno (startLineno t) 0
            blankPrev =
                "|" ++ showLineno line 0 ++ " " ++ replicate (length prev - (linenoDigits + 2)) '-'
            showAll customPrev =
                concat $ map (\x -> showTrees (startLineno t) customPrev [x]) ts

    showLineno n pad = printf ("%" ++ show (linenoDigits + pad) ++ "d") n

    startLineno (Tree ((_, p) : _) _) = lineno p
    startLineno _                     = undefined
    linenoDigits = length $ show $ programLines
    lineno p = programLines - nlines p + 1
    programLines = nlines (program tree)
    nlines = length . lines
    program (Tree [(_, p)] _) = p
    program _                 = undefined

parseStr' :: String -> IO (Either (InterpError, DebugTree, Env) ([Value], DebugTree, Env))
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

parseStr :: String -> IO (Either (InterpError, DebugTree, Env) [Value])
parseStr s = do
    content <- parseStr' s
    return $ do
        (val, _, _) <- content
        return val

runStr :: String -> IO (Either (InterpError, DebugTree, Env) Value)
runStr s = do
    content <- parseStr' s
    content' <- case content of
        Right (exprs, _, env) -> runEval (env, emptyTree) $ mapM eval exprs
        Left err              -> return $ Left err
    return $ case content' of
        Right (vals, _, _) -> Right $ last vals
        Left err           -> Left err

debugFile :: String -> IO ()
debugFile fname = do
    file <- readFile fname
    parsing <- parseStr' file
    case parsing of
        Right (vals, tree, env) -> do
            print (ListVal vals)
            showEnv env
            showTree tree
        Left (err, tree, env) -> do
            putStrLn $ "Error: " ++ show err
            showEnv env
            showTree tree
    where
        showTree tree = do
            putStrLn "\n=== DEBUG OUTPUT ==="
            putStrLn $ showDebugTree tree
        showEnv (Env _ _ (EnvMap env)) = do
            putStrLn "\n=== FINAL ENV ===\n"
            putStrLn $ showMap 0 env
        showMap k env = intercalate "\n\n" (map (showVar k) $ Map.toList env)
        showVar k (Ident name _, Right val)         = (indent k) ++ name ++ ": " ++ show val
        showVar k (Ident name _, Left (EnvMap env)) =
            (indent k) ++ name ++ ":\n" ++ showMap (k + 1) env
        indent k = replicate (2 * k) ' '
