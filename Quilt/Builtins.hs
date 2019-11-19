module Quilt.Builtins (
    builtins,
) where

import Quilt.Value
import Quilt.Eval
import Quilt.Parser

import Control.Monad.Except

builtins :: [(String, Value)]
builtins = [("+", function $ intBinOp (+)),
            ("-", function $ intBinOp (-)),
            ("parse-str", function parseStr)]

function :: ([Value] -> Eval Value) -> Value
function f = PrimFunc $ \args -> mapM eval args >>= f

intBinOp :: (Integer -> Integer -> Integer) -> [Value] -> Eval Value
intBinOp op [IntVal x, IntVal y] = return $ IntVal (op x y)
intBinOp _ _ = throwError InvalidArguments

parseStr :: [Value] -> Eval Value
parseStr [StringVal m, StringVal s, v] = eval $ FuncCall (parserToVal p) [StringVal s, v]
    where p = parseString m >> return (StringVal m)
parseStr _ = throwError InvalidArguments
