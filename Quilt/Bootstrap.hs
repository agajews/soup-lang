module Quilt.Bootstrap (
    initType,
    initEnv,
) where

import Quilt.Value
import Quilt.Env
import Quilt.Eval
import Quilt.Parse
import Quilt.Parser

import Control.Monad.Except
import Data.Char

initEnv :: Env
initEnv = emptyEnv

topRules :: [Parser Value]
topRules = [parseInt, parseIntPlus]

-- initTypes :: [Value]
-- initTypes = [initType]

-- typeToVar :: Value -> Eval Value
-- typeToVar t = do
--     n <- genIdent
--     setVar n $ ListVal [ListVal t]
--     return $ Variable n

initType :: Eval Value
initType = return $ ListVal [ListVal $ map parserToValue topRules]

parseInt :: Parser Value
parseInt = Parser $ \s -> case span isDigit s of
    ((x:xs), rest) -> [(IntVal $ read (x:xs), rest)]
    _ -> []

parseChar :: Char -> Parser ()
parseChar c = Parser $ \s -> case s of
    (c':rest) -> if c' == c then [((), rest)] else []
    _ -> []

parseString :: String -> Parser ()
parseString s = sequence_ $ map parseChar s

parseIntPlus :: Parser Value
parseIntPlus = do
    x <- parseInt
    parseChar '+'
    y <- parseInt
    return $ FuncCall (PrimFunc intSum) [x, y]

intSum :: [Value] -> Eval Value
intSum [IntVal x, IntVal y] = return $ IntVal (x + y)
intSum _ = throwError InvalidArguments
