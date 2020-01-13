{-# LANGUAGE FlexibleContexts #-}

module Soup.Bootstrap (
    initType,
) where

import Soup.Builtins
import Soup.Env
import Soup.Eval
import Soup.Parse
import Soup.Parser
import Soup.Value

import Control.Applicative
import Control.Monad.Except

import Data.Char
import qualified Data.Map as Map

import Debug.Trace

initType :: Eval Value
initType = do
    pexpType <- genIdent "pexp"
    let pexpFun = parserToVal "name:pexp" $ pexpParser pexpType
    let lambdaFun = parserToVal "lambda" $ lambdaParser pexpType
    let funcCallFun = parserToVal "func-call" $ parseFuncCall pexpType
    let runFun = parserToVal "run" $ parseRun pexpType

    topType <- genIdent "top"
    let topFun = parserToVal "name:top" $ topTypeParser topType

    let intFun = parserToVal "int" parseInt
    let strFun = parserToVal "str" parseStr
    let builtinParsers = map builtinParser builtins

    setVar pexpType $ ListVal $ [
        pexpFun,
        topFun,
        lambdaFun,
        funcCallFun,
        runFun,
        intFun,
        strFun] ++ builtinParsers
    setVar topType $ ListVal [parserToVal "pexp" $ parseType pexpType]

    return $ ListVal [parserToVal "top" $ topParser topType]

pexpParser :: Ident -> Parser Value
pexpParser n = literalParser "pexp" $ Variable n

topTypeParser :: Ident -> Parser Value
topTypeParser n = literalParser "top" $ Variable n

topParser :: Ident -> Parser Value
topParser topType = do
    parseNewlines'
    vals <- parseInterspersed (parseType topType) parseNewlines
    parseNewlines'
    return $ ListVal vals

literalParser :: String -> Value -> Parser Value
literalParser s v = parseString s >> (logDebug $ "`" ++ s ++ "`") >> return v

lambdaParser :: Ident -> Parser Value
lambdaParser pexp = do
    parseString "(lambda"

    logDebug "\\"

    parseWS
    parseString "("

    paramNames <- parseInterspersed' parseIdent parseWS
    paramIdents <- liftEval $ mapM genIdent paramNames
    let paramParsers = zipWith literalParser paramNames (map Variable paramIdents)

    parseString ")"
    parseWS

    liftEval $ modifyVar pexp (pushRules paramNames paramParsers)
    body <- parseType pexp
    liftEval $ modifyVar pexp popRules

    parseString ")"

    return $ Lambda paramIdents body

pushRules :: [String] -> [Parser Value] -> Value -> Eval Value
pushRules names ps l@(ListVal _) = do
    return $ ListVal $ (zipWith parserToVal names ps) ++ [l]
pushRules _ _ v = throwError $ InvalidType v

popRules :: Value -> Eval Value
popRules v = do
    popRules' v
    where
        popRules' (ListVal (x:y:ys))    = popRules' (ListVal (y:ys))
        popRules' (ListVal [ListVal l]) = return $ ListVal l
        popRules' v                     = throwError $ InvalidType v

parseFuncCall :: Ident -> Parser Value
parseFuncCall pexp = do
    parseString "("
    logDebug "()"
    fun <- parseType pexp
    args <- catchFail (return []) $ do
        parseWS
        parseInterspersed (parseType pexp) parseWS
    parseString ")"
    return $ FuncCall fun args

parseRun :: Ident -> Parser Value
parseRun pexp = do
    parseString "(run"
    logDebug "run"
    parseWS
    expr <- parseType pexp
    parseString ")"
    liftEval $ eval expr

parseInt :: Parser Value
parseInt = do
    digits <- parseWhile isDigit
    logDebug $ show digits
    return $ IntVal $ read digits

parseStr :: Parser Value
parseStr = do
    parseString "\""
    logDebug "\""
    s <- takeString
    logDebug $ "\"" ++ s ++ "\""
    return $ StringVal s
    where
        takeString = do
            c <- takeChar
            if null c
            then return c
            else do
                s <- takeString
                return $ c ++ s

        takeChar = Parser $ \s c -> case s of
            ('"':xs) -> c "" xs
            ('\\':x:xs) -> if Map.member x codes
                then c [codes Map.! x] xs
                else c [x] xs
            ('\\':[]) -> return []
            (x:xs) -> c [x] xs
            "" -> return []

        codes = Map.fromList [('b', '\b'), ('n', '\n'), ('f', '\f'), ('r', '\r'),
                              ('t', '\t'), ('\\', '\\'), ('\"', '\"')]

isWhitespace :: Char -> Bool
isWhitespace = (`elem` [' ', '\n', '\t'])

parseWS :: Parser String
parseWS = parseWhile isWhitespace

parseWS' :: Parser String
parseWS' = parseWhile' isWhitespace

parseNewlines :: Parser String
parseNewlines = parseWhile (== '\n')

parseNewlines' :: Parser String
parseNewlines' = parseWhile' (== '\n')

parseIdent :: Parser String
parseIdent = do
    name <- parseWhile $ liftM2 (||) isAlphaNum (`elem` "~!@#$%^&*-=+_|'<>?")
    guard $ not . null . filter (not . isDigit) $ name
    return name

builtinParser :: (String, Value) -> Value
builtinParser (name, f) = parserToVal name $ literalParser name f
