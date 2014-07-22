-- |
-- Module    : Statistics.Matrix.Mutable
-- Copyright : (c) 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Basic mutable matrix operations.

module Statistics.Matrix.Mutable
    (
      MMatrix(..)
    , MVector
    , replicate
    , thaw
    , bounds
    , unsafeFreeze
    , unsafeRead
    , unsafeWrite
    , unsafeModify
    , immutably
    , unsafeBounds
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.Monad.ST (ST)
import Statistics.Matrix.Types (Matrix(..), MMatrix(..), MVector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Prelude hiding (replicate)

replicate :: Int -> Int -> Double -> ST s (MMatrix s)
replicate r c k = MMatrix r c 0 <$> M.replicate (r*c) k

thaw :: Matrix -> ST s (MMatrix s)
thaw (Matrix r c e v) = MMatrix r c e <$> U.thaw v

unsafeFreeze :: MMatrix s -> ST s Matrix
unsafeFreeze (MMatrix r c e mv) = Matrix r c e <$> U.unsafeFreeze mv

unsafeRead :: MMatrix s -> Int -> Int -> ST s Double
unsafeRead mat r c = unsafeBounds mat r c M.unsafeRead
{-# INLINE unsafeRead #-}

unsafeWrite :: MMatrix s -> Int -> Int -> Double -> ST s ()
unsafeWrite mat row col k = unsafeBounds mat row col $ \v i ->
  M.unsafeWrite v i k
{-# INLINE unsafeWrite #-}

unsafeModify :: MMatrix s -> Int -> Int -> (Double -> Double) -> ST s ()
unsafeModify mat row col f = unsafeBounds mat row col $ \v i -> do
  k <- M.unsafeRead v i
  M.unsafeWrite v i (f k)
{-# INLINE unsafeModify #-}

-- | Given row and column numbers, calculate the offset into the flat
-- row-major vector.
bounds :: MMatrix s -> Int -> Int -> (MVector s -> Int -> r) -> r
bounds (MMatrix rs cs _ mv) r c k
  | r < 0 || r >= rs = error "row out of bounds"
  | c < 0 || c >= cs = error "column out of bounds"
  | otherwise        = k mv $! r * cs + c
{-# INLINE bounds #-}

-- | Given row and column numbers, calculate the offset into the flat
-- row-major vector, without checking.
unsafeBounds :: MMatrix s -> Int -> Int -> (MVector s -> Int -> r) -> r
unsafeBounds (MMatrix _ cs _ mv) r c k = k mv $! r * cs + c
{-# INLINE unsafeBounds #-}

immutably :: NFData a => MMatrix s -> (Matrix -> a) -> ST s a
immutably mmat f = do
  k <- f <$> unsafeFreeze mmat
  rnf k `seq` return k
{-# INLINE immutably #-}
