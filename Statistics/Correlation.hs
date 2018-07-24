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

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Statistics.Matrix
import Statistics.Sample
import Statistics.Types         (partial,StatisticsException(..))
import Statistics.Test.Internal (rankUnsorted)


----------------------------------------------------------------
-- Pearson
----------------------------------------------------------------

-- | Pearson correlation for sample of pairs. Exactly same as
-- 'Statistics.Sample.correlation'
pearson :: (G.Vector v (Double, Double), G.Vector v Double, MonadThrow m)
        => v (Double, Double) -> m Double
pearson = correlation
{-# INLINE pearson #-}

-- | Compute pairwise pearson correlation between rows of a matrix
pearsonMatByRow :: MonadThrow m => Matrix -> m Matrix
pearsonMatByRow m
  | nC < 2    = throwM $ InvalidSample "Statistics.Correlation.pearsonMatByRow" "Insufficient sample size"
  | otherwise = return $ generateSym (rows m)
      (\i j -> partial $ pearson $ row m i `U.zip` row m j)
  where
    (_, nC) = dimension m
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
            , MonadThrow m
            )
         => v (a, b)
         -> m Double
spearman xy
  = pearson
  $ G.zip (rankUnsorted x) (rankUnsorted y)
  where
    (x, y) = G.unzip xy
{-# INLINE spearman #-}

-- | compute pairwise spearman correlation between rows of a matrix
spearmanMatByRow :: MonadThrow m => Matrix -> m Matrix
spearmanMatByRow m
  | nC < 2    = throwM $ InvalidSample "Statistics.Correlation.pearsonMatByRow" "Insufficient sample size"
  | otherwise = pearsonMatByRow $ fromRows $ fmap rankUnsorted $ toRows m
  where
    (_, nC) = dimension m
{-# INLINE spearmanMatByRow #-}
