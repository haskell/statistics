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
    , fromLambda
    ) where

import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed     as U
import qualified Statistics.Distribution as D
import Statistics.Math (logGamma, factorial)

newtype PoissonDistribution = PD {
      pdLambda :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution PoissonDistribution where
    cumulative  = D.cdfFromProbability
    {-# INLINE cumulative #-}

instance D.DiscreteDistr PoissonDistribution where
    probability = probability

instance D.Variance PoissonDistribution where
    variance = pdLambda
    {-# INLINE variance #-}

instance D.Mean PoissonDistribution where
    mean = pdLambda
    {-# INLINE mean #-}

fromLambda :: Double -> PoissonDistribution
fromLambda = PD
{-# INLINE fromLambda #-}

probability :: PoissonDistribution -> Int -> Double
probability (PD l) n
  | n < 0                   = 0
  | l < 20 && n <= 100      = exp (-l) * l ** x / factorial n
  | otherwise               = exp (x * log l - logGamma (x + 1) - l)
    where
      x = fromIntegral n
{-# INLINE probability #-}
