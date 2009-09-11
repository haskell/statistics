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
-- The Geometric distribution.  This is the discrete probability
-- distribution of a number of events occurring in a fixed interval if
-- these events occur with a known average rate, and occur
-- independently from each other within that interval.

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
    probability = probability
    cumulative  = cumulative
    inverse     = inverse

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

probability :: GeometricDistribution -> Double -> Double
probability (GD s) x = s * (1-s) ** (x-1)
{-# INLINE probability #-}

cumulative :: GeometricDistribution -> Double -> Double
cumulative (GD s) x = 1 - (1-s) ** x
{-# INLINE cumulative #-}

inverse :: GeometricDistribution -> Double -> Double
inverse (GD s) p = log (1 - p) / log (1 - s)
{-# INLINE inverse #-}
