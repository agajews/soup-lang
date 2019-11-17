{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Quilt.NondetStateT (
    NondetStateT(..),
    fork,
) where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

newtype NondetStateT s m a = NondetStateT { runNondetStateT :: (s -> m [(a, s)]) }

instance (Monad m) => Monad (NondetStateT s m) where
    return x = NondetStateT $ \s -> return [(x, s)]
    (NondetStateT x) >>= f = NondetStateT $ \s -> do
        l <- x s
        liftM concat $ sequence [runNondetStateT (f v) s' | (v, s') <- l]

instance MonadTrans (NondetStateT s) where
    lift m = NondetStateT $ \s -> do
        x <- m
        return [(x, s)]

instance MonadError e m => MonadError e (NondetStateT s m) where
    throwError = lift . throwError
    catchError m h = NondetStateT $ \s -> runNondetStateT m s `catchError` \e -> runNondetStateT (h e) s

instance Monad m => MonadState s (NondetStateT s m) where
    get = NondetStateT $ \s -> return [(s, s)]
    put s' = NondetStateT $ \s -> return [((), s')]
    -- state f = NondetStateT $ \s -> return [f s]

instance Monad m => Applicative (NondetStateT s m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (NondetStateT s m) where
    fmap = liftM

fork :: Monad m => [a] -> NondetStateT s m a
fork xs = NondetStateT $ \s -> return $ map (\x -> (x, s)) xs
