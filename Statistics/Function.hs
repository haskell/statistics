{-# LANGUAGE Rank2Types, TypeOperators #-}
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
    , indices
    -- * Array setup
    , createU
    , createIO
    ) where

import Control.Exception (assert)
import Control.Monad.ST (ST, unsafeIOToST, unsafeSTToIO)
import Data.Vector.Algorithms.Combinators (apply)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Generic (unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.Vector.Algorithms.Intro as I

-- | Sort an array.
sort :: (U.Unbox e, Ord e) => U.Vector e -> U.Vector e
sort = apply I.sort
{-# INLINE sort #-}

-- | Partially sort an array, such that the least /k/ elements will be
-- at the front.
partialSort :: (U.Unbox e, Ord e) =>
               Int              -- ^ The number /k/ of least elements.
            -> U.Vector e
            -> U.Vector e
partialSort k = apply (\a -> I.partialSort a k)
{-# INLINE partialSort #-}

-- | Return the indices of an array.
indices :: (U.Unbox a) => U.Vector a -> U.Vector Int
indices a = U.enumFromTo 0 (U.length a - 1)
{-# INLINE indices #-}

data MM = MM {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the minimum and maximum of an array in one pass.
minMax :: U.Vector Double -> (Double , Double)
minMax = fini . U.foldl go (MM (1/0) (-1/0))
  where
    go (MM lo hi) k = MM (min lo k) (max hi k)
    fini (MM lo hi) = (lo , hi)
{-# INLINE minMax #-}

-- | Create an array, using the given 'ST' action to populate each
-- element.
createU :: (U.Unbox e) => forall s. Int -> (Int -> ST s e) -> ST s (U.Vector e)
createU size itemAt = assert (size >= 0) $
    MU.new size >>= loop 0
  where
    loop k arr | k >= size = unsafeFreeze arr
               | otherwise = do r <- itemAt k
                                MU.write arr k r
                                loop (k+1) arr
{-# INLINE createU #-}

-- | Create an array, using the given 'IO' action to populate each
-- element.
createIO :: (U.Unbox e) => Int -> (Int -> IO e) -> IO (U.Vector e)
createIO size itemAt =
    unsafeSTToIO $ createU size (unsafeIOToST . itemAt)
{-# INLINE createIO #-}
