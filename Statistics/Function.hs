{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Vector.Generic (unsafeFreeze)
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

-- | Sort a vector.
sort :: (Ord e, G.Vector v e) => v e -> v e
sort = apply I.sort
{-# INLINE sort #-}

-- | Partially sort a vector, such that the least /k/ elements will be
-- at the front.
partialSort :: (G.Vector v e, Ord e) =>
               Int -- ^ The number /k/ of least elements.
            -> v e
            -> v e
partialSort k = apply (\a -> I.partialSort a k)
{-# INLINE partialSort #-}

-- | Return the indices of a vector.
indices :: (G.Vector v a, G.Vector v Int) => v a -> v Int
indices a = G.enumFromTo 0 (G.length a - 1)
{-# INLINE indices #-}

-- | Zip a vector with its indices.
indexed :: (G.Vector v e, G.Vector v Int, G.Vector v (Int,e)) => v e -> v (Int,e)
indexed a = G.zip (indices a) a
{-# INLINE indexed #-}

data MM = MM {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the minimum and maximum of a vector in one pass.
minMax :: (G.Vector v Double) => v Double -> (Double, Double)
minMax = fini . G.foldl' go (MM (1/0) (-1/0))
  where
    go (MM lo hi) k = MM (min lo k) (max hi k)
    fini (MM lo hi) = (lo, hi)
{-# INLINE minMax #-}

-- | Create a vector, using the given action to populate each
-- element.
create :: (PrimMonad m, G.Vector v e) => Int -> (Int -> m e) -> m (v e)
create size itemAt = assert (size >= 0) $
    M.new size >>= loop 0
  where
    loop k arr | k >= size = unsafeFreeze arr
               | otherwise = do r <- itemAt k
                                M.write arr k r
                                loop (k+1) arr
{-# INLINE create #-}
