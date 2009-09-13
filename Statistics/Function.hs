{-# LANGUAGE TypeOperators #-}
-- |
-- Module    : Statistics.Function
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Useful functions.

module Statistics.Function
    (
      minMax
    , sort
    , partialSort
    , createU
    ) where

import Control.Exception (assert)
import Control.Monad.ST (unsafeSTToIO)
import Data.Array.Vector.Algorithms.Combinators (apply)
import Data.Array.Vector
import qualified Data.Array.Vector.Algorithms.Intro as I

-- | Sort an array.
sort :: (UA e, Ord e) => UArr e -> UArr e
sort = apply I.sort
{-# INLINE sort #-}

-- | Partially sort an array, such that the least /k/ elements will be
-- at the front.
partialSort :: (UA e, Ord e) =>
               Int              -- ^ The number /k/ of least elements.
            -> UArr e
            -> UArr e
partialSort k = apply (\a -> I.partialSort a k)
{-# INLINE partialSort #-}

data MM = MM {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the minimum and maximum of an array in one pass.
minMax :: UArr Double -> Double :*: Double
minMax = fini . foldlU go (MM (1/0) (-1/0))
  where
    go (MM lo hi) k = MM (min lo k) (max hi k)
    fini (MM lo hi) = lo :*: hi
{-# INLINE minMax #-}

-- | Create an array, using the given action to populate each element.
createU :: (UA e) => Int -> (Int -> IO e) -> IO (UArr e)
createU size itemAt = assert (size >= 0) $
    unsafeSTToIO (newMU size) >>= loop 0
  where
    loop k arr | k >= size = unsafeSTToIO (unsafeFreezeAllMU arr)
               | otherwise = do
      r <- itemAt k
      unsafeSTToIO (writeMU arr k r)
      loop (k+1) arr
