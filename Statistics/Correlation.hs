{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Statistics.Correlation.Pearson
--
module Statistics.Correlation
    ( -- * Pearson correlation
      pearson
    , pearsonMatByRow
      -- * Spearman correlation
    , spearman
    , spearmanMatByRow
    ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Statistics.Matrix
import Statistics.Sample
import Statistics.Test.Internal (rankUnsorted)


----------------------------------------------------------------
-- Pearson
----------------------------------------------------------------

-- | Pearson correlation for sample of pairs.
pearson :: (G.Vector v (Double, Double), G.Vector v Double)
        => v (Double, Double) -> Double
pearson = correlation
{-# INLINE pearson #-}

-- | Compute pairwise pearson correlation between rows of a matrix
pearsonMatByRow :: Matrix -> Matrix
pearsonMatByRow m
  = generateSym (rows m)
      (\i j -> pearson $ row m i `U.zip` row m j)
{-# INLINE pearsonMatByRow #-}



----------------------------------------------------------------
-- Spearman
----------------------------------------------------------------

-- | compute spearman correlation between two samples
spearman :: ( Ord a
            , Ord b
            , G.Vector v a
            , G.Vector v b
            , G.Vector v (a, b)
            , G.Vector v Int
            , G.Vector v Double
            , G.Vector v (Double, Double)
            , G.Vector v (Int, a)
            , G.Vector v (Int, b)
            )
         => v (a, b)
         -> Double
spearman xy
  = pearson
  $ G.zip (rankUnsorted x) (rankUnsorted y)
  where
    (x, y) = G.unzip xy
{-# INLINE spearman #-}

-- | compute pairwise spearman correlation between rows of a matrix
spearmanMatByRow :: Matrix -> Matrix
spearmanMatByRow
  = pearsonMatByRow . fromRows . fmap rankUnsorted . toRows
{-# INLINE spearmanMatByRow #-}
