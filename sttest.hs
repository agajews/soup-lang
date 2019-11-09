{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST
import Data.STRef

-- x = newSTRef 5

-- y = do
--     xref <- x
--     writeSTRef xref 10
--     readSTRef xref

-- z = do
--     xref <- x
--     writeSTRef xref 6
--     readSTRef xref

-- xST = newSTRef 5

-- f :: (forall s. ST s (STRef s Int)) -> Int
-- f xST = runST $ do
--     x <- xST
--     readSTRef x

-- x = f (newSTRef 5)

-- f :: (forall s. ST s Int) -> ST s Int
-- f x = x

-- g :: (forall s. ST s Int) -> Int
-- g x = runST (f x)

-- f :: (forall s. ST s (STRef s Int) -> ST s (STRef s Int))
-- f st = st

-- g :: (forall s. ST s (STRef s Int) -> ST s (STRef s Int))
-- g st = st

-- h :: (forall s. ST s (STRef s Int)) -> ST s (STRef s Int)
-- h = g . f

newtype IntBox = IntBox { unboxInt :: forall s. ST s (STRef s Int) }

f :: IntBox -> [ST s (STRef s Int)]
f x = [unboxInt x]

g :: IntBox -> ST s (STRef s Int)
g x = unboxInt x

mapIntBox :: [ST s (STRef s Int)] -> [IntBox]
mapIntBox (x:xs) = IntBox x : mapIntBox xs

h :: IntBox -> [ST s (STRef s Int)]
h x = map g (mapIntBox (f x))
