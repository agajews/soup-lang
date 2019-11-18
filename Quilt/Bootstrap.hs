module Quilt.Bootstrap (
    initType,
) where

import Quilt.Value
import Quilt.Env
import Quilt.Eval
import Quilt.Parse
import Quilt.Parser
import Quilt.Builtins

import Control.Monad.Except
import Data.Char

initType :: Eval Value
initType = do
    pexpType <- genIdent
    let pexpFun = parserToVal $ pexpParser pexpType
    let lambdaFun = parserToVal $ lambdaParser pexpType
    topType <- genIdent
    let topFun = parserToVal $ topTypeParser topType
    let intFun = parserToVal parseInt
    let builtinParsers = map builtinParser builtins

    setVar pexpType $ ListVal $ [pexpFun, topFun, lambdaFun, intFun] ++ builtinParsers
    setVar topType $ ListVal [pexpFun]

    return $ ListVal [parserToVal $ topParser topType]

pexpParser :: Ident -> Parser
pexpParser n = parseString "pexp" >> return (Variable n)

topTypeParser :: Ident -> Parser
topTypeParser n = parseString "top" >> return (Variable n)

topParser :: Ident -> Parser
topParser topType = do
    parseNewlines'
    vals <- parseInterspersed (parseType topType) parseNewlines
    parseNewlines'
    return $ last vals

literalParser :: String -> Value -> Parser
literalParser s v = parseString s >> return v

lambdaParser :: Ident -> Parser
lambdaParser pexp = do
    parseString "(lambda"

    parseWS
    parseString "("

    paramNames <- parseInterspersed' parseIdent parseWS
    paramIdents <- liftEval $ sequence $ replicate (length paramNames) genIdent
    let paramParsers = liftM2 literalParser paramNames (map Variable paramIdents)

    parseString ")"
    parseWS

    liftEval $ modifyVar pexp (pushRules paramParsers)
    body <- parseType pexp
    liftEval $ modifyVar pexp popRules

    parseString ")"

    return $ Lambda paramIdents body

pushRules :: [Parser] -> Value -> Eval Value
pushRules ps l@(ListVal _) = return $ ListVal $ (map parserToVal ps) ++ [l]
pushRules _ _ = throwError InvalidRule

popRules :: Value -> Eval Value
popRules (ListVal [ListVal l]) = return $ ListVal l
popRules (ListVal (v:vs)) = popRules (ListVal vs)
popRules _ = throwError InvalidRule

isWhitespace :: Char -> Bool
isWhitespace = (`elem` [' ', '\n', '\t'])

parseWS :: Parser
parseWS = parseWhile isWhitespace 

parseWS' :: Parser
parseWS' = parseWhile' isWhitespace

parseNewlines :: Parser
parseNewlines = parseWhile (== '\n')

parseNewlines' :: Parser
parseNewlines' = parseWhile' (== '\n')

parseIdent :: Parser
parseIdent = parseWhile $ liftM2 (||) isAlphaNum (`elem` "~!@#$%^&*-=+_|'<>?")

parseInt :: Parser
parseInt = do
    digit <- parseWhile isDigit
    return $ IntVal $ read digit

builtinParser :: (String, [Value] -> Eval Value) -> Value
builtinParser (name, f) = parserToVal $ literalParser name (PrimFunc f)
