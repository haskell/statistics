{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, Rank2Types #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
-- |
-- Module    : Statistics.Function
-- Copyright : (c) 2009, 2010, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Useful functions.

module Statistics.Function
    (
    -- * Scanning
      minMax
    -- * Sorting
    , sort
    , gsort
    , sortBy
    , partialSort
    -- * Indexing
    , indexed
    , indices
    -- * Bit twiddling
    , nextHighestPowerOfTwo
    -- * Comparison
    , within
    -- * Arithmetic
    , square
    -- * Vectors
    , unsafeModify
    -- * Combinators
    , for
    , rfor
    ) where

#include "MachDeps.h"

import Control.Monad.ST (ST)
import Data.Bits ((.|.), shiftR)
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric.MathFunctions.Comparison (within)

-- | Sort a vector.
sort :: U.Vector Double -> U.Vector Double
sort = G.modify I.sort
{-# NOINLINE sort #-}

-- | Sort a vector.
gsort :: (Ord e, G.Vector v e) => v e -> v e
gsort = G.modify I.sort
{-# INLINE gsort #-}

-- | Sort a vector using a custom ordering.
sortBy :: (G.Vector v e) => I.Comparison e -> v e -> v e
sortBy f = G.modify $ I.sortBy f
{-# INLINE sortBy #-}

-- | Partially sort a vector, such that the least /k/ elements will be
-- at the front.
partialSort :: (G.Vector v e, Ord e) =>
               Int -- ^ The number /k/ of least elements.
            -> v e
            -> v e
partialSort k = G.modify (`I.partialSort` k)
{-# SPECIALIZE partialSort :: Int -> U.Vector Double -> U.Vector Double #-}

-- | Return the indices of a vector.
indices :: (G.Vector v a, G.Vector v Int) => v a -> v Int
indices a = G.enumFromTo 0 (G.length a - 1)
{-# INLINE indices #-}

-- | Zip a vector with its indices.
indexed :: (G.Vector v e, G.Vector v (Int,e)) => v e -> v (Int,e)
indexed xs = G.imap (,) xs
{-# INLINE indexed #-}

data MM = MM {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the minimum and maximum of a vector in one pass.
minMax :: (G.Vector v Double) => v Double -> (Double, Double)
minMax = fini . G.foldl' go (MM (1/0) (-1/0))
  where
    go (MM lo hi) k = MM (min lo k) (max hi k)
    fini (MM lo hi) = (lo, hi)
{-# INLINE minMax #-}

-- | Efficiently compute the next highest power of two for a
-- non-negative integer.  If the given value is already a power of
-- two, it is returned unchanged.  If negative, zero is returned.
nextHighestPowerOfTwo :: Int -> Int
nextHighestPowerOfTwo n
#if WORD_SIZE_IN_BITS == 64
  = 1 + _i32
#else
  = 1 + i16
#endif
  where
    i0   = n - 1
    i1   = i0  .|. i0  `shiftR` 1
    i2   = i1  .|. i1  `shiftR` 2
    i4   = i2  .|. i2  `shiftR` 4
    i8   = i4  .|. i4  `shiftR` 8
    i16  = i8  .|. i8  `shiftR` 16
    _i32 = i16 .|. i16 `shiftR` 32
-- It could be implemented as
--
-- > nextHighestPowerOfTwo n = 1 + foldl' go (n-1) [1, 2, 4, 8, 16, 32]
--     where go m i = m .|. m `shiftR` i
--
-- But GHC do not inline foldl (probably because it's recursive) and
-- as result function walks list of boxed ints. Hand rolled version
-- uses unboxed arithmetic.

-- | Multiply a number by itself.
square :: Double -> Double
square x = x * x

-- | Simple for loop.  Counts from /start/ to /end/-1.
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 !n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = f i >> loop (i+1)
{-# INLINE for #-}

-- | Simple reverse-for loop.  Counts from /start/-1 to /end/ (which
-- must be less than /start/).
rfor :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rfor n0 !n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = let i' = i-1 in f i' >> loop i'
{-# INLINE rfor #-}

unsafeModify :: M.MVector s Double -> Int -> (Double -> Double) -> ST s ()
unsafeModify v i f = do
  k <- M.unsafeRead v i
  M.unsafeWrite v i (f k)
{-# INLINE unsafeModify #-}
