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
import Statistics.Distribution
import Statistics.Distribution.StudentT
import Statistics.Matrix
import Statistics.Sample
import Statistics.Test.Internal (rankUnsorted)
import Statistics.Types (mkPValue, PValue)


----------------------------------------------------------------
-- Pearson
----------------------------------------------------------------

-- | Pearson correlation for sample of pairs. Exactly same as
-- 'Statistics.Sample.correlation'
pearson :: (G.Vector v (Double, Double), G.Vector v Double)
        => v (Double, Double) -> (Double, PValue Double)
pearson xy = (coeff, p)
  where
    coeff = correlation xy
    n     = fromIntegral . G.length $ xy
    stat  = coeff * ((sqrt (n - 2)) / (1 - (coeff ** 2)))
    p     = mkPValue $ 2 * (complCumulative (studentT (n - 2)) . abs $ stat)
{-# INLINE pearson #-}

-- | Compute pairwise pearson correlation between rows of a matrix
pearsonMatByRow :: Matrix -> Matrix
pearsonMatByRow m
  = generateSym (rows m)
      (\i j -> fst . pearson $ row m i `U.zip` row m j)
{-# INLINE pearsonMatByRow #-}



----------------------------------------------------------------
-- Spearman
----------------------------------------------------------------

-- | Compute spearman correlation between two samples with p value. P value is
-- calculated using Student's /t/ distribution with /n - 2/ degrees of freedom
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
         -> (Double, PValue Double)
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
