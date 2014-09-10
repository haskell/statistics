{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Statistics.Correlation.Pearson
--

module Statistics.Correlation.Pearson
    ( pearson
    ) where

import Statistics.Sample
import qualified Data.Vector.Generic as G

-- | calculate the Pearson's correlation between two samples
pearson :: G.Vector v Double => v Double -> v Double -> Double
pearson x y | G.length x /= G.length y = error "Statistics.Correlation.Pearson.pearson: Incompatible dimensions"
            | otherwise = cov / sqrt (var_x * var_y)
  where
    (m_x, var_x) = meanVarianceUnb x
    (m_y, var_y) = meanVarianceUnb y
    cov = G.sum (G.zipWith (\a b -> (a - m_x) * (b - m_y)) x y) / (n - 1)
    n = fromIntegral . G.length $ x
{-# INLINE pearson #-}
