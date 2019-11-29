module Quilt.Bootstrap (
    initType,
) where

import Quilt.Value
import Quilt.Env
import Quilt.Eval
import Quilt.Parse
import Quilt.Parser
import Quilt.Builtins

import Control.Applicative
import Control.Monad.Except

import Data.Char
import qualified Data.Map as Map

import Debug.Trace

initType :: Eval Value
initType = do
    pexpType <- genIdent
    let pexpFun = parserToVal "parser:pexp-name" $ pexpParser pexpType
    let lambdaFun = parserToVal "parser:lambda" $ lambdaParser pexpType
    let funcCallFun = parserToVal "parser:func-call" $ parseFuncCall pexpType
    let runFun = parserToVal "run" $ parseRun pexpType

    topType <- genIdent
    let topFun = parserToVal "top" $ topTypeParser topType

    let intFun = parserToVal "parser:int" parseInt
    let strFun = parserToVal "parser:str" parseStr
    let builtinParsers = map builtinParser builtins

    setVar pexpType $ ListVal $ [pexpFun, lambdaFun, funcCallFun, runFun, topFun, intFun, strFun] ++ builtinParsers
    setVar topType $ ListVal [parserToVal "parser:pexp" $ parseType pexpType]

    return $ ListVal [parserToVal "parser:top" $ topParser topType]

pexpParser :: Ident -> Parser Value
pexpParser n = parseString "pexp" >> return (Variable n)

topTypeParser :: Ident -> Parser Value
topTypeParser n = parseString "top" >> return (Variable n)

topParser :: Ident -> Parser Value
topParser topType = do
    parseNewlines'
    vals <- parseInterspersed (parseType topType) parseNewlines
    parseNewlines'
    return $ ListVal vals

literalParser :: String -> Value -> Parser Value
literalParser s v = parseString s >> return v

lambdaParser :: Ident -> Parser Value
lambdaParser pexp = do
    parseString "(lambda"

    parseWS
    parseString "("

    paramNames <- parseInterspersed' parseIdent parseWS
    paramIdents <- liftEval $ sequence $ replicate (length paramNames) genIdent
    let paramParsers = zipWith literalParser paramNames (map Variable paramIdents)

    parseString ")"
    parseWS

    liftEval $ modifyVar pexp (pushRules paramNames paramParsers)
    body <- parseType pexp
    liftEval $ modifyVar pexp popRules

    parseString ")"

    return $ Lambda paramIdents body

pushRules :: [String] -> [Parser Value] -> Value -> Eval Value
pushRules names ps l@(ListVal _) = return $ ListVal $ (zipWith parserToVal names ps) ++ [l]
pushRules _ _ _ = throwError InvalidRule

popRules :: Value -> Eval Value
popRules (ListVal [ListVal l]) = return $ ListVal l
popRules (ListVal (v:vs)) = popRules (ListVal vs)
popRules _ = throwError InvalidRule

parseFuncCall :: Ident -> Parser Value
parseFuncCall pexp = do
    parseString "("
    fun <- parseType pexp
    args <- catchFail (return []) $ do
        parseWS
        parseInterspersed (parseType pexp) parseWS
    parseString ")"
    return $ FuncCall fun args

parseRun :: Ident -> Parser Value
parseRun pexp = do
    parseString "(run"
    parseWS
    expr <- parseType pexp
    parseString ")"
    liftEval $ eval expr

parseInt :: Parser Value
parseInt = do
    digit <- parseWhile isDigit
    return $ IntVal $ read digit

parseStr :: Parser Value
parseStr = do
    parseString "\""
    s <- parseMany' $ parseIf (not . (== '\\')) <|> parseEscape
    parseString "\""
    return $ StringVal s
    where
        parseEscape = do
            parseString "\\"
            c <- parseIf (`Map.member` codes)
            return $ codes Map.! c
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
