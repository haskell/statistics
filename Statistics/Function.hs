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
    ) where

import Data.Array.Vector.Algorithms.Combinators (apply)
import Data.Array.Vector ((:*:)(..), UA, UArr, foldlU)
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
