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

import Control.Exception
import qualified Data.Vector.Unboxed as U (Vector)

-- | Sample data.
type Sample = U.Vector Double

-- | Sample with weights. First element of sample is data, second is weight
type WeightedSample = U.Vector (Double,Double)

-- | Weights for affecting the importance of elements of a sample.
type Weights = U.Vector Double

-- | Generic exception for use in statistics package
data StatisticsException
  = InvalidSample String String
    -- ^ Quantity of interest couldn't be calculated for given
    --   sample. Parameters are function name and human-readable error
    --   string.
  | ProbabilityOutOf_01_Range String
    -- ^ Probability is out of 0,1 range
  | TestFailure String String
  deriving (Show)

instance Exception StatisticsException
