{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Poisson
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Poisson distribution.  This is the discrete probability
-- distribution of a number of events occurring in a fixed interval if
-- these events occur with a known average rate, and occur
-- independently from each other within that interval.

module Statistics.Distribution.Poisson
    (
      PoissonDistribution
    -- * Constructors
    , poisson
    -- * Accessors
    , poissonLambda
    -- * References
    -- $references
    ) where

import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Poisson.Internal as I

newtype PoissonDistribution = PD {
      poissonLambda :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution PoissonDistribution where
    cumulative d x = D.sumProbabilities d 0 (floor x)
    {-# INLINE cumulative #-}

instance D.DiscreteDistr PoissonDistribution where
    probability (PD lambda) x = I.probability lambda (fromIntegral x)
    {-# INLINE probability #-}

instance D.Variance PoissonDistribution where
    variance = poissonLambda
    {-# INLINE variance #-}

instance D.Mean PoissonDistribution where
    mean = poissonLambda
    {-# INLINE mean #-}

-- | Create Poisson distribution.
poisson :: Double -> PoissonDistribution
poisson l
  | l <= 0    = error $ "Statistics.Distribution.Poisson.poisson:\
                        \ lambda must be positive. Got " ++ show l
  | otherwise = PD l
{-# INLINE poisson #-}

-- $references
--
-- * Loader, C. (2000) Fast and Accurate Computation of Binomial
--   Probabilities. <http://projects.scipy.org/scipy/raw-attachment/ticket/620/loader2000Fast.pdf>
