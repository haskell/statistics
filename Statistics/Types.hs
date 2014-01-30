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
      Estimator(..)
    , Sample
    , WeightedSample
    , Weights
    ) where

import qualified Data.Vector.Unboxed as U (Vector)

-- | Sample data.
type Sample = U.Vector Double

-- | Sample with weights. First element of sample is data, second is weight
type WeightedSample = U.Vector (Double,Double)

-- | An estimator of a property of a sample, such as its 'mean'.
--
-- The use of an algebraic data type here allows functions such as
-- 'jackknife' and 'bootstrapBCA' to use more efficient algorithms
-- when possible.
data Estimator = Mean
               | Variance
               | VarianceUnbiased
               | StdDev
               | Function (Sample -> Double)

-- | Weights for affecting the importance of elements of a sample.
type Weights = U.Vector Double
