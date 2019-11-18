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
    let pexpFun = parserToValue $ pexpParser pexpType
    let lambdaFun = parserToValue $ lambdaParser pexpType
    topType <- genIdent
    let topFun = parserToValue $ topTypeParser topType
    let intFun = parserToValue parseInt
    let builtinParsers = map builtinParser builtins

    setVar pexpType $ ListVal $ [pexpFun, topFun, lambdaFun, intFun] ++ builtinParsers
    setVar topType $ ListVal [pexpFun]

    return $ ListVal [parserToValue $ topParser topType]

pexpParser :: Ident -> Parser Value
pexpParser n = parseString "pexp" >> return (Variable n)

topTypeParser :: Ident -> Parser Value
topTypeParser n = parseString "top" >> return (Variable n)

topParser :: Ident -> Parser Value
topParser topType = do
    parseNewlines'
    vals <- parseMany $ do
        parseType topType $ \x -> do
            parseNewlines
            return x
    return $ last vals

literalParser :: String -> Value -> Parser Value
literalParser s v = parseString s >> return v

lambdaParser :: Ident -> Parser Value
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
    parseType pexp $ \body -> do
        liftEval $ modifyVar pexp popRules
        parseString ")"
        return $ Lambda paramIdents body

pushRules :: [Parser Value] -> Value -> Eval Value
pushRules ps l@(ListVal _) = return $ ListVal $ (map parserToValue ps) ++ [l]
pushRules _ _ = throwError InvalidRule

popRules :: Value -> Eval Value
popRules (ListVal [ListVal l]) = return $ ListVal l
popRules (ListVal (v:vs)) = popRules (ListVal vs)
popRules _ = throwError InvalidRule

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

parseInt :: Parser Value
parseInt = do
    digit <- parseWhile isDigit
    return $ IntVal $ read digit

builtinParser :: (String, [Value] -> Eval Value) -> Value
builtinParser (name, f) = parserToValue $ literalParser name (PrimFunc f)
