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
    pexpFun <- parserToValue $ pexpParser pexpType
    lambdaFun <- parserToValue $ lambdaParser pexpType
    topType <- genIdent
    topFun <- parserToValue $ topTypeParser topType
    intFun <- parserToValue parseInt
    setVar pexpType $ ListVal [pexpFun, topFun, lambdaFun, intFun]
    setVar topType $ ListVal [pexpFun]

    mapM setupBuiltin builtins

    return $ ListVal [parserToValue $ topParser topType]

pexpParser :: Ident -> Parser Value
pexpParser n = parseString "pexp" >> return (Variable n)

topTypeParser :: Ident -> Parser Value
topTypeParser n = parseString "top" >> return (Variable n)

topParser :: Ident -> Parser Value
topParser topType = parseNewlines' >> parseMany $ do
    x <- parseType topType
    parseNewlines
    return x

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
parseIdent = parseWhile $ liftM2 (||) isAlphaNum (`elem` "~!@#$%^&*-=+_|'<>?")

literalParser :: String -> Value -> Parser Value
literalParser s v -> parseString s >> return v

lambdaParser :: Ident -> Parser Value
lambdaParser pexp = do
    parseString "(lambda"
    parseWS

    parseString "("
    paramNames <- parseInterspersed' parseIdent parseWS
    paramIdents <- sequence $ replicate (length paramNames) genIdent
    let paramParsers = liftM2 literalParser paramNames (map Variable paramIdents)
    parseString ")"

    parseWS
    liftEval $ modifyVar pexp (pushRules paramParsers)
    body <- parseType pexp
    liftEval $ modifyVar pexp popRules
    parseString ")"

    return $ Lambda paramIdents body

pushRules :: [Parser Value] -> Value -> Value
pushRules ps l@(ListVal _) = ListVal $ (map parserToValue ps) ++ [l]
pushRules _ _ = InvalidRule

popRules :: Value -> Value
popRules (ListVal [ListVal l]) = ListVal l
popRules (ListVal (v:vs)) = popRules (ListVal vs)
popRules _ = InvalidRule

parseInt :: Parser Value
parseInt = do
    digit <- parseWhile isDigit
    return $ read digit

setupBuiltin :: (String, [Value] -> Eval Value) -> Eval Value
setupBuiltin (name, f) = literalParser name (primFun f)
