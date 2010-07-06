{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Geometric
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Geometric distribution. This is the probability distribution of
-- the number of Bernoulli trials needed to get one success, supported
-- on the set [1,2..].
--
-- This distribution is sometimes referred to as the /shifted/
-- geometric distribution, to distinguish it from a variant measuring
-- the number of failures before the first success, defined over the
-- set [0,1..].

module Statistics.Distribution.Geometric
    (
      GeometricDistribution
    -- * Constructors
    , fromSuccess
    -- ** Accessors
    , pdSuccess
    ) where

import Control.Exception (assert)
import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D

newtype GeometricDistribution = GD {
      pdSuccess :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution GeometricDistribution where
    cumulative = cumulative

instance D.DiscreteDistr GeometricDistribution where
    probability = probability

instance D.Variance GeometricDistribution where
    variance (GD s) = (1 - s) / (s * s)
    {-# INLINE variance #-}

instance D.Mean GeometricDistribution where
    mean (GD s) = 1 / s
    {-# INLINE mean #-}

fromSuccess :: Double -> GeometricDistribution
fromSuccess x = assert (x >= 0 && x <= 1)
                GD x
{-# INLINE fromSuccess #-}

probability :: GeometricDistribution -> Int -> Double
probability (GD s) n = s * (1-s) ** (fromIntegral n - 1)
{-# INLINE probability #-}

cumulative :: GeometricDistribution -> Double -> Double
cumulative (GD s) x | x < 0     = 0
                    | otherwise = 1 - (1-s) ^ (floor x)
{-# INLINE cumulative #-}
