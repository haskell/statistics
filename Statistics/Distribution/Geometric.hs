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
    density    = density
    cumulative = cumulative
    quantile   = quantile

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

density :: GeometricDistribution -> Double -> Double
density (GD s) x = s * (1-s) ** (x-1)
{-# INLINE density #-}

cumulative :: GeometricDistribution -> Double -> Double
cumulative (GD s) x = 1 - (1-s) ** x
{-# INLINE cumulative #-}

quantile :: GeometricDistribution -> Double -> Double
quantile (GD s) p = log (1 - p) / log (1 - s)
{-# INLINE quantile #-}
