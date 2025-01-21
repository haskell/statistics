{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Statistics.Correlation.Pearson
--
module Statistics.Correlation
    ( -- * Pearson correlation
      pearson
    , pearson2
    , pearsonMatByRow
      -- * Spearman correlation
    , spearman
    , spearman2
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

-- | Pearson correlation for sample of pairs. Exactly same as
-- 'Statistics.Sample.correlation'
pearson :: (G.Vector v (Double, Double))
        => v (Double, Double) -> Double
pearson = correlation
{-# INLINE pearson #-}

-- | Pearson correlation for sample of pairs. Exactly same as
-- 'Statistics.Sample.correlation'
pearson2 :: (G.Vector v Double)
         => v Double -> v Double -> Double
pearson2 = correlation2
{-# INLINE pearson2 #-}

-- | Compute pairwise Pearson correlation between rows of a matrix
pearsonMatByRow :: Matrix -> Matrix
pearsonMatByRow m
  = generateSym (rows m)
      (\i j -> pearson $ row m i `U.zip` row m j)
{-# INLINE pearsonMatByRow #-}



----------------------------------------------------------------
-- Spearman
----------------------------------------------------------------

-- | Compute Spearman correlation between two samples
spearman :: ( Ord a
            , Ord b
            , G.Vector v a
            , G.Vector v b
            , G.Vector v (a, b)
            , G.Vector v Int
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

-- | Compute Spearman correlation between two samples. Samples must
--   have same length.
spearman2 :: ( Ord a
            , Ord b
            , G.Vector v a
            , G.Vector v b
            , G.Vector v Int
            , G.Vector v (Int, a)
            , G.Vector v (Int, b)
            )
         => v a
         -> v b
         -> Double
spearman2 xs ys
  | nx /= ny  = error "Statistics.Correlation.spearman2: samples must have same length"
  | otherwise = pearson $ G.zip (rankUnsorted xs) (rankUnsorted ys)
  where
    nx = G.length xs
    ny = G.length ys
{-# INLINE spearman2 #-}

-- | compute pairwise Spearman correlation between rows of a matrix
spearmanMatByRow :: Matrix -> Matrix
spearmanMatByRow
  = pearsonMatByRow . fromRows . fmap rankUnsorted . toRows
{-# INLINE spearmanMatByRow #-}
