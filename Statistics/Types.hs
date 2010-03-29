-- |
-- Module    : Statistics.Types
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Types for working with statistics.

module Statistics.Types
    (
      Estimator
    , Sample
    , WeightedSample
    , Weights
    ) where

import qualified Data.Vector.Unboxed as U (Vector)

-- | Sample data.
type Sample = U.Vector Double

-- | Sample with weights. First element of sample is data, second is weight
type WeightedSample = U.Vector (Double,Double)

-- | A function that estimates a property of a sample, such as its
-- 'mean'.
type Estimator = Sample -> Double

-- | Weights for affecting the importance of elements of a sample.
type Weights = U.Vector Double
