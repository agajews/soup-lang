module Reader (
    demo,
)
where

import Control.Monad
import Control.Applicative

-- newtype Reader a = Reader (String -> (a, String))

-- instance Monad Reader where
--     return x = Reader $ \s -> (x, s)
--     r >>= f = Reader $ \s -> apply (f x) s' where (x, s') = apply r s


data Reader a = Reader a String
    deriving (Show)

instance Monad Reader where
    return x = Reader x Nothing
    Reader x Nothing >>= f = Reader x' s' where Reader x' s' = f x
    Reader x (Just s) >>= f = Reader x' (s <|> s') where Reader x' s' = f x

instance Applicative Reader where
    pure = return
    (<*>) = ap

instance Functor Reader where
    fmap = liftM

load :: String -> Reader ()
load s = Reader () (Just s)

readChar :: Reader a -> Reader Char
readChar Reader x Nothing = 

-- data Writer a = Writer a String
--     deriving (Show)

-- instance Monad Writer where
--     return x = Writer x ""
--     Writer x s >>= f = Writer x' (s ++ s') where Writer x' s' = f x

-- instance Applicative Writer where
--     pure = return
--     (<*>) = ap

-- instance Functor Writer where
--     fmap = liftM

-- write :: String -> Writer ()
-- write s = Writer () s

-- computeAndWrite :: Writer Int
-- computeAndWrite = do
--     write "Computing :)"
--     return $ 10 + 10

-- demo :: Writer Int
-- demo = do
--     write "Hello"
--     x <- computeAndWrite
--     write "World"
--     y <- computeAndWrite
--     return $ x * y
