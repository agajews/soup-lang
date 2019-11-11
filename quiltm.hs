{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

import Data.Char
import Data.Maybe
import qualified Data.Map as Map


data InterpError = UnboundVariable Ident
                 | TooFewArgs
                 | TooManyArgs
                 | InvalidRule
                 | InvalidType
                 | InvalidArguments
                 | ParsingError
                 | AmbiguousParsing
    deriving (Show)

data Env = Env Integer (Map.Map Ident Value)

emptyEnv :: Env
emptyEnv = Env 0 Map.empty

genIdent :: (MonadState Env m) => m Ident
genIdent = do
    Env n env <- get
    let n' = n + 1
    put $ Env n' env
    return $ Ident n'

getVar :: (MonadState Env m, MonadError InterpError m) => Ident -> m Value
getVar n = do
    Env _ env <- get
    case Map.lookup n env of
        Just v -> return v
        Nothing -> throwError (UnboundVariable n)

setVar :: (MonadState Env m) => Ident -> Value -> m ()
setVar n v = do
    Env m env <- get
    put $ Env m (Map.insert n v env)
    return ()

newtype Eval a = Eval { unwrapEval :: StateT Env (ExceptT InterpError Identity) a }
    deriving (Functor,
              Applicative,
              Monad,
              MonadError InterpError,
              MonadState Env)

runEval :: Env -> Eval a -> Either InterpError (a, Env)
runEval env x = runIdentity (runExceptT (runStateT (unwrapEval x) env))

newtype Ident = Ident Integer
    deriving (Show, Eq, Ord)

data Value = StringVal String
           | IntVal Integer
           | ListVal [Value]
           | PrimFunc ([Value] -> Eval Value)
           | LambdaVal [Ident] Value
           | Variable Ident
           | FuncCall Value [Value]

instance Show Value where
    show (StringVal x) = "StringVal " ++ show x
    show (IntVal x) = "IntVal " ++ show x
    show (ListVal x) = "ListVal " ++ show x
    show (PrimFunc _) = "PrimFunc"
    show (LambdaVal ps b) = "Lambda " ++ show ps ++ " " ++ show b
    show (Variable n) = show n
    show (FuncCall v args) = "FuncCall " ++ show v ++ " " ++ show args

localState :: Eval a -> Eval a
localState v = do
    env <- get
    case runEval env v of
        Right (v, _) -> return v
        Left err -> throwError err

runRule :: String -> Eval Value -> Eval [(Eval Value, String)]
runRule s v = do
    p <- localState parsing
    return [(parsing >> return v, s') | (v, s') <- p]
    where
        parsing = v >>= (\x -> eval $ FuncCall x [StringVal s]) >>= extractParsing
        extractParsing :: Value -> Eval [(Value, String)]
        extractParsing (ListVal (ListVal [v, StringVal s'] : rest)) = do
            l <- extractParsing (ListVal rest)
            return $ (v, s') : l
        extractParsing (ListVal []) = return []
        extractParsing _ = throwError InvalidRule

extractRules :: Eval Value -> Eval [Eval Value]
extractRules v = do
    rs <- localState rules 
    return [rules >> return r | r <- rs]
    where
        extractType (ListVal v) = return v
        extractType _ = throwError InvalidType
        rules = v >>= eval >>= extractType

parse :: String -> Eval Value -> Eval [(Eval Value, String)]
parse s v = do
    rules <- extractRules v
    parsings <- mapM (runRule s) rules
    return $ concat parsings

parseStr :: String -> Either InterpError Value
parseStr s = do
    (parsings, _) <- runEval emptyEnv $ parse s initType
    case filter (null . snd) parsings of
        [(v, _)] -> do
            (value, _) <- runEval emptyEnv v
            return value
        [] -> Left ParsingError
        _ -> Left AmbiguousParsing
    
runStr :: String -> Either InterpError Value
runStr s = do
    expr <- parseStr s
    (val, _) <- runEval emptyEnv $ eval expr
    return val

initType :: Eval Value
initType = return $ ListVal $ map parserToValue [parseInt,
                                                 parseIntPlus]

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Monad Parser where
    return x = Parser $ \s -> [(x, s)]
    p >>= f = Parser $ \s -> concat [(runParser $ f x) s' | (x, s') <- runParser p s]

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM

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

parserToValue :: Parser Value -> Value
parserToValue (Parser p) = PrimFunc $ \x -> case x of
    [StringVal s] -> return . ListVal . map (\(v, s') -> ListVal [v, StringVal s']) $ p s
    _ -> throwError InvalidArguments

eval :: Value -> Eval Value
eval x@(StringVal _) = return x
eval x@(IntVal _) = return x
eval x@(ListVal _) = return x
eval x@(PrimFunc _) = return x
eval x@(LambdaVal _ _) = return x
eval (Variable n) = getVar n
eval (FuncCall fExp argExps) = do
    fVal <- eval fExp
    args <- mapM eval argExps
    case fVal of
        PrimFunc f -> f args
        LambdaVal params body -> matchArgs params args >> eval body

matchArgs :: [Ident] -> [Value] -> Eval ()
matchArgs (n:ns) (v:vs) = setVar n v >> matchArgs ns vs
matchArgs [] [] = return ()
matchArgs _ [] = throwError TooFewArgs
matchArgs [] _ = throwError TooManyArgs
