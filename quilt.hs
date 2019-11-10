{-# LANGUAGE RankNTypes #-}

module Quilt (
    Value(..),
) where

import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.List
import Data.Either
import Data.Maybe

data Value s = StringVal String
             | IntVal Integer
             | ListVal [Value s]
             | Variable (STRef s (Maybe (Value s)))
             | FuncCall (Value s) [Value s]
             | PrimFunc ([Value s] -> EitherS (Value s))
             | Lambda { params :: [STRef s (Maybe (Value s))],
                        body :: [Value s]}

newtype ValueBox = ValueBox { unboxValue :: forall s. ST s (Value s) }

extractRight :: Either a b -> b
extractRight (Right x) = x
extractRight _ = undefined

extractJust :: Maybe a -> a
extractJust (Just x) = x
extractJust _ = undefined

processRule :: Value s -> EitherS (Maybe (String, Value s))
processRule (ListVal [StringVal s, v]) = Right $ Just (s, v)
processRule (ListVal []) = Right $ Nothing
processRule _ = Left "bad rule"

evalRule :: String -> Value s -> ST s (EitherS (Value s))
evalRule s v = eval (FuncCall v [StringVal s])

runRule :: String -> ValueBox -> EitherS (Maybe (String, ValueBox))
runRule s vBox = (liftM . liftM) genOutput s' where
    tupST :: ST s (EitherS (Maybe (String, Value s)))
    tupST = liftM (>>= processRule) (unboxValue vBox >>= evalRule s)

    s' :: EitherS (Maybe String)
    s' = runST $ (liftM . liftM . liftM) fst tupST 

    genOutput :: String -> (String, ValueBox)
    genOutput s' = (s', ValueBox $ liftM (snd . extractJust . extractRight) tupST)

processType :: Value s -> EitherS [Value s]
processType (ListVal l) = Right l
processType _ = Left "bad type"

unsequenceVB :: (forall s. ST s [Value s]) -> [ValueBox]
unsequenceVB m = ValueBox (liftM head m) : unsequenceVB (liftM tail m)

extractRules :: ValueBox -> EitherS [ValueBox]
extractRules vBox = do
    n <- runST $ liftM (liftM length . processType) $ unboxValue vBox
    return $ take n $ unsequenceVB $ liftM (extractRight . processType) $ unboxValue vBox

parse :: String -> ValueBox -> EitherS [(String, ValueBox)]
parse s vBox = do
    rules <- extractRules vBox
    parsings <- mapM (runRule s) rules
    return [p | Just p <- parsings]

newtype Parser a = Parser { runParser :: String -> EitherS [(String, a)] }

instance Monad Parser where
    return x = Parser $ \s -> Right [(s, x)]
    p >>= f = Parser $ \s -> do
        l <- (runParser p) s
        res <- sequence [(runParser $ f x) s' | (s', x) <- l]
        return $ concat res

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

-- parseM :: ValueBox -> Parser ValueBox
-- parseM s vBox = hoistEither $ parse s vBox

-- intSum :: [Value s] -> EitherS (Value s)
-- intSum [StringVal s] = do

-- intPlus :: [Value s] -> EitherS (Value s)
-- intPlus [IntVal a, IntVal b] = Right $ IntVal $ a + b
-- intPlus _ = Left "invalid args to plus"

initType :: ValueBox
initType = ValueBox $ return $ ListVal []

parseStr :: String -> EitherS String
parseStr s = do
    parsings <- parse s initType
    case filter (null . fst) parsings of
        [(_, v)] -> Right $ runST $ liftM show (unboxValue v)
        _ -> Left "parsing error"

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
    Lambda { params = ps, body = bs } -> if (length ps) == (length args) then do
        forM (zip ps args) $ \(p, a) -> writeSTRef p (Just a)
        liftM last . mapM eval $ bs
        else return $ Left $ concat ["number of arguments ",
                                     "(", show (length args), ") ",
                                     "does not match number of parameters ",
                                     "(", show (length ps), ")"]
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

-- demo :: String
-- demo = runST $ do
--     res <- eval $ FuncCall (PrimFunc plus) [IntVal 1, IntVal 1]
--     return $ show res
