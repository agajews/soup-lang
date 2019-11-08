module Quilt (
    Value(..),
    Rule,
    Type,
    demo,
) where

import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.List

data Value s = StringVal String
             | IntVal Integer
             | ListVal [Value s]
             | Variable (STRef s (Maybe (Value s)))
             | FuncCall (Value s) [Value s]
             | PrimFunc ([Value s] -> EitherS (Value s))
             | Lambda { params :: [STRef s (Maybe (Value s))],
                        body :: [Value s]}

data Rule s = Rule { runRule :: String -> ST s [(String, ST s (Value s))] }
data Type s = Type (STRef s [Rule s])

parse :: Type s -> String -> ST s [(String, ST s (Value s))]
parse (Type t) s = do
    rules <- readSTRef t
    results <- sequence $ map (`runRule` s) rules
    return $ concat results

initType :: ST s (Type s)
initType = do
    rules <- newSTRef []
    return $ Type rules

parseStr :: String -> String
parseStr s = runST $ do
    t <- initType
    results <- parse t s
    output <- case filter (null . fst) results of
        (_, v):_ -> v >>= eval
        _ -> return $ Left "parsing error"
    return $ show output

instance Show (Value s) where
    show (StringVal x) = show x
    show (IntVal x) = show x
    show (ListVal x) = show x
    show (Variable _) = "var"
    show (FuncCall f as) = show f ++ "(" ++ intercalate ", " (map show as) ++ ")"
    show (PrimFunc f) = "primfun"
    show Lambda { body = bs } = "lambda{" ++ intercalate ", " (map show bs) ++ "}"

type EitherS = Either String

evalArgs :: [Value s] -> ST s (EitherS [Value s])
evalArgs = liftM sequence . mapM eval

applyFunc :: (Value s) -> [Value s] -> ST s (EitherS (Value s))
applyFunc f args = case f of
    PrimFunc p -> return $ p args
    Lambda { params = ps, body = bs } -> do
        forM (zip ps args) $ \(p, a) -> writeSTRef p (Just a)
        liftM last . mapM eval $ bs
    _ -> return $ Left "attempting to call a non-function value"
        
eval :: Value s -> ST s (EitherS (Value s))
eval x@(StringVal _) = return $ Right x
eval x@(IntVal _) = return $ Right x
eval x@(ListVal _) = return $ Right x
eval x@(PrimFunc _) = return $ Right x
eval x@(Lambda _ _) = return $ Right x
eval (Variable ref) = do
    contents <- readSTRef ref
    return $ case contents of
        Nothing -> Left "undefined variable"
        Just val -> Right val
eval (FuncCall fExp argExps) = do
    fEither <- eval fExp
    argsEither <- evalArgs argExps
    let r = do
        args <- argsEither
        f <- fEither
        return $ applyFunc f args
    case r of
        Right v -> v
        Left err -> return $ Left err

plus :: [Value s] -> EitherS (Value s)
plus [IntVal a, IntVal b] = Right $ IntVal $ a + b
plus _ = Left "invalid args to plus"

demo :: String
demo = runST $ do
    res <- eval $ FuncCall (PrimFunc plus) [IntVal 1, IntVal 1]
    return $ show res
