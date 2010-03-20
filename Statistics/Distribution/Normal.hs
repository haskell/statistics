{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Normal
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The normal distribution.  This is a continuous probability
-- distribution that describes data that cluster around a mean.

module Statistics.Distribution.Normal
    (
      NormalDistribution
    -- * Constructors
    , fromParams
    , fromSample
    , standard
    ) where

import Control.Exception (assert)
import Data.Number.Erf (erfc)
import Data.Typeable (Typeable)
import Statistics.Constants (m_sqrt_2, m_sqrt_2_pi)
import qualified Statistics.Distribution as D
import qualified Statistics.Sample as S

-- | The normal distribution.
data NormalDistribution = ND {
      mean     :: {-# UNPACK #-} !Double
    , variance :: {-# UNPACK #-} !Double
    , ndPdfDenom :: {-# UNPACK #-} !Double
    , ndCdfDenom :: {-# UNPACK #-} !Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution NormalDistribution where
    density    = density
    cumulative = cumulative
    quantile   = quantile

instance D.Variance NormalDistribution where
    variance = variance

instance D.Mean NormalDistribution where
    mean = mean

-- | Standard normal distribution with mean equal to 0 and variance equal to 1
standard :: NormalDistribution
standard = ND {
             mean = 0.0
           , variance = 1.0
           , ndPdfDenom = m_sqrt_2_pi
           , ndCdfDenom = m_sqrt_2
           }

-- | Create normal distribution from parameters
fromParams :: Double            -- ^ Mean of distribution
           -> Double            -- ^ Variance of distribution
           -> NormalDistribution
fromParams m v = assert (v > 0)
                 ND {
                   mean = m
                 , variance = v
                 , ndPdfDenom = m_sqrt_2_pi * sv
                 , ndCdfDenom = m_sqrt_2 * sv
                 }
    where sv = sqrt v

-- | Create distribution using parameters estimated from
--   sample. Variance is estimated using maximum likelihood method
--   (biased estimation).
fromSample :: S.Sample -> NormalDistribution
fromSample a = fromParams (S.mean a) (S.variance a)

density :: NormalDistribution -> Double -> Double
density d x = exp (-xm * xm / (2 * variance d)) / ndPdfDenom d
    where xm = x - mean d

cumulative :: NormalDistribution -> Double -> Double
cumulative d x = erfc (-(x-mean d) / ndCdfDenom d) / 2

quantile :: NormalDistribution -> Double -> Double
quantile d p
  | p < 0 || p > 1 = inf/inf
  | p == 0         = -inf
  | p == 1         = inf
  | p == 0.5       = mean d
  | otherwise      = x * sqrt (variance d) + mean d
  where x          = D.findRoot standard p 0 (-100) 100
        inf        = 1/0
