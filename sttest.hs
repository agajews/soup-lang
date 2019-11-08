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

f :: (forall s. ST s (STRef s Int)) -> Int
f xST = runST $ do
    x <- xST
    readSTRef x
