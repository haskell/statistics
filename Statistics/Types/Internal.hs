-- |
-- Module    : Statistics.Types.Internal
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Types for working with statistics.
module Statistics.Types.Internal where


import qualified Data.Vector.Unboxed as U (Vector)

-- | Sample data.
type Sample = U.Vector Double

-- | Sample with weights. First element of sample is data, second is weight
type WeightedSample = U.Vector (Double,Double)

-- | Weights for affecting the importance of elements of a sample.
type Weights = U.Vector Double

