{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Statistics.Correlation.Spearman
--
module Statistics.Correlation.Spearman
    ( spearman
    , spearmanMatByRow
    ) where

import qualified Data.Vector.Generic as G
import Statistics.Correlation.Pearson
import Statistics.Matrix hiding (map)
import Statistics.Test.Internal (rankUnsorted)


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
  = pearsonMatByRow . fromRows . map rankUnsorted . toRows
{-# INLINE spearmanMatByRow #-}
