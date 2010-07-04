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

import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Distribution as D
import Statistics.Constants (m_huge)
import Statistics.Math (factorial, logGamma)

newtype PoissonDistribution = PD {
      pdLambda :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution PoissonDistribution where
    density    = density
    cumulative = cumulative
    quantile   = quantile

instance D.Variance PoissonDistribution where
    variance = pdLambda
    {-# INLINE variance #-}

instance D.Mean PoissonDistribution where
    mean = pdLambda
    {-# INLINE mean #-}

fromLambda :: Double -> PoissonDistribution
fromLambda = PD
{-# INLINE fromLambda #-}

density :: PoissonDistribution -> Double -> Double
density (PD l) x
    | x < 0                   = 0
    | l >= 100 && x >= l * 10 = 0
    | l >= 3 && x >= l * 100  = 0
    | x >= max 1 l * 200      = 0
    | l < 20 && x <= 100      = exp (-l) * l ** x / factorial (floor x)
    | otherwise               = x * log l - logGamma (x + 1) - l
{-# INLINE density #-}

cumulative :: PoissonDistribution -> Double -> Double
cumulative d = U.sum . U.map (density d . fromIntegral) .
               U.enumFromTo (0::Int) . floor
{-# INLINE cumulative #-}

quantile :: PoissonDistribution -> Double -> Double
quantile d p = fromIntegral . r $ D.findRoot d p (pdLambda d) 0 m_huge
    where r = round :: Double -> Int
{-# INLINE quantile #-}
