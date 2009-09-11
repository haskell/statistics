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
    -- , fromSample
    ) where

import Data.Array.Vector
import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import Statistics.Constants (m_huge)
import Statistics.Math (logGamma)

newtype PoissonDistribution = PD {
      pdLambda :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution PoissonDistribution where
    probability = probability
    cumulative  = cumulative
    inverse     = inverse

instance D.Variance PoissonDistribution where
    variance = pdLambda
    {-# INLINE variance #-}

instance D.Mean PoissonDistribution where
    mean = pdLambda
    {-# INLINE mean #-}

fromLambda :: Double -> PoissonDistribution
fromLambda = PD
{-# INLINE fromLambda #-}

probability :: PoissonDistribution -> Double -> Double
probability (PD l) x = exp (x * log l - l - logGamma x)
{-# INLINE probability #-}

cumulative :: PoissonDistribution -> Double -> Double
cumulative d = sumU . mapU (probability d . fromIntegral) .
               enumFromToU (0::Int) . floor
{-# INLINE cumulative #-}

inverse :: PoissonDistribution -> Double -> Double
inverse d p = fromIntegral . r $ D.findRoot d p (pdLambda d) 0 m_huge
    where r = round :: Double -> Int
{-# INLINE inverse #-}
