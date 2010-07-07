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
    ) where

import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import Statistics.Math (logGamma, factorial)

newtype PoissonDistribution = PD {
      poissonLambda :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution PoissonDistribution where
    cumulative d x = D.sumProbabilities d 0 (floor x)
    {-# INLINE cumulative #-}

instance D.DiscreteDistr PoissonDistribution where
    probability = probability

instance D.Variance PoissonDistribution where
    variance = poissonLambda
    {-# INLINE variance #-}

instance D.Mean PoissonDistribution where
    mean = poissonLambda
    {-# INLINE mean #-}

-- | Create po
poisson :: Double -> PoissonDistribution
poisson = PD
{-# INLINE poisson #-}

probability :: PoissonDistribution -> Int -> Double
probability (PD l) n
  | n < 0                   = 0
  | l < 20 && n <= 100      = exp (-l) * l ** x / factorial n
  | otherwise               = exp (x * log l - logGamma (x + 1) - l)
    where
      x = fromIntegral n
{-# INLINE probability #-}
