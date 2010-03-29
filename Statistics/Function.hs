{-# LANGUAGE Rank2Types, TypeOperators #-}
-- |
-- Module    : Statistics.Function
-- Copyright : (c) 2009, 2010 Bryan O'Sullivan
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
    , indexed
    , indices
    -- * Vector setup
    , create
    ) where

import Control.Exception (assert)
import Control.Monad.Primitive (PrimMonad)
import Data.Vector.Algorithms.Combinators (apply)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Generic (unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.Vector.Algorithms.Intro as I

-- | Sort a vector.
sort :: (U.Unbox e, Ord e) => U.Vector e -> U.Vector e
sort = apply I.sort
{-# INLINE sort #-}

-- | Partially sort a vector, such that the least /k/ elements will be
-- at the front.
partialSort :: (U.Unbox e, Ord e) =>
               Int              -- ^ The number /k/ of least elements.
            -> U.Vector e
            -> U.Vector e
partialSort k = apply (\a -> I.partialSort a k)
{-# INLINE partialSort #-}

-- | Return the indices of a vector.
indices :: (U.Unbox a) => U.Vector a -> U.Vector Int
indices a = U.enumFromTo 0 (U.length a - 1)
{-# INLINE indices #-}

-- | Zip a vector with its indices.
indexed :: U.Unbox e => U.Vector e -> U.Vector (Int,e)
indexed a = U.zip (indices a) a
{-# INLINE indexed #-}

data MM = MM {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the minimum and maximum of a vector in one pass.
minMax :: U.Vector Double -> (Double , Double)
minMax = fini . U.foldl go (MM (1/0) (-1/0))
  where
    go (MM lo hi) k = MM (min lo k) (max hi k)
    fini (MM lo hi) = (lo, hi)
{-# INLINE minMax #-}

-- | Create a vector, using the given action to populate each
-- element.
create :: (PrimMonad m, U.Unbox e) => Int -> (Int -> m e) -> m (U.Vector e)
create size itemAt = assert (size >= 0) $
    MU.new size >>= loop 0
  where
    loop k arr | k >= size = unsafeFreeze arr
               | otherwise = do r <- itemAt k
                                MU.write arr k r
                                loop (k+1) arr
{-# INLINE create #-}
