{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Gamma
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The gamma distribution.  This is a continuous probability
-- distribution with two parameters, /k/ and &#977;. If /k/ is
-- integral, the distribution represents the sum of /k/ independent
-- exponentially distributed random variables, each of which has a
-- mean of &#977;.

module Statistics.Distribution.Gamma
    (
      GammaDistribution
    -- * Constructors
    --, fromParams
    --, fromSample
    --, standard
    -- * Accessors
    , gdShape
    , gdScale
    ) where

import Data.Typeable (Typeable)
import Statistics.Constants (m_huge)
import Statistics.Math (incompleteGamma, logGamma)
import qualified Statistics.Distribution as D

-- | The gamma distribution.
data GammaDistribution = GD {
      gdShape :: {-# UNPACK #-} !Double -- ^ Shape parameter, /k/.
    , gdScale :: {-# UNPACK #-} !Double -- ^ Scale parameter, &#977;.
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution GammaDistribution where
    density    = density
    cumulative = cumulative
    quantile   = quantile

instance D.Variance GammaDistribution where
    variance (GD a l) = a / (l * l)
    {-# INLINE variance #-}

instance D.Mean GammaDistribution where
    mean (GD a l) = a / l
    {-# INLINE mean #-}

density :: GammaDistribution -> Double -> Double
density (GD a l) x = x ** (a-1) * exp (-x/l) / (exp (logGamma a) * l ** a)
{-# INLINE density #-}

cumulative :: GammaDistribution -> Double -> Double
cumulative (GD a l) x = incompleteGamma a (x/l) / exp (logGamma a)
{-# INLINE cumulative #-}

quantile :: GammaDistribution -> Double -> Double
quantile d p
  | p == 0    = -1/0
  | p == 1    = 1/0
  | otherwise = D.findRoot d p (gdShape d) 0 m_huge
{-# INLINE quantile #-}
