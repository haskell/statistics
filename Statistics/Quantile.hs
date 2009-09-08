-- |
-- Module    : Statistics.Quantile
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for approximating quantiles.

module Statistics.Quantile
    (
     quantile
    ) where

import Control.Exception (assert)
import Data.Array.Vector (allU, indexU, lengthU)
import Statistics.Types (Sample, Sorted(..))

-- | Use the weighted average method to estimate the @k@th
-- @q@-quantile of a set of samples.
quantile :: Int -> Int -> Sorted Sample -> Double
quantile k q (Sorted a) =
    assert (q >= 2) .
    assert (k >= 0) .
    assert (k < q) .
    assert (allU (not . isNaN) a) $
    aj + g * (aj1 - aj)
  where
    j   = floor idx
    idx = fromIntegral (lengthU a - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    aj  = indexU a j
    aj1 = indexU a (j+1)
