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
      choose
    , minMax
    , sort
    , partialSort
    ) where

import Data.Array.Vector.Algorithms.Immutable (apply)
import Data.Array.Vector ((:*:)(..), UA, UArr, enumFromToU, foldlU)
import qualified Data.Array.Vector.Algorithms.Intro as I

-- | Sort.
sort :: (UA e, Ord e) => UArr e -> UArr e
sort = apply I.sort
{-# INLINE sort #-}

-- | Partially sort, such that the least @k@ elements will be
-- at the front.
partialSort :: (UA e, Ord e) =>
               Int              -- ^ The number @k@ of least elements
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

-- | The binomial coefficient.
--
-- > 7 `choose` 3 == 35
choose :: Int -> Int -> Int
n `choose` k
    | k > n = 0
    | otherwise = ceiling . foldlU go 1 . enumFromToU 1 $ k'
    where go a i = a * (nk + j) / j
              where j = fromIntegral i :: Double
          k' | k > n `div` 2 = n - k
             | otherwise     = k
          nk = fromIntegral (n - k')
{-# INLINE choose #-}
