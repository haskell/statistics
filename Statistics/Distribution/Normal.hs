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
    , normalDistr
    , normalFromSample
    , standard
    ) where

import Data.Number.Erf (erfc)
import Data.Typeable (Typeable)
import Statistics.Constants (m_sqrt_2, m_sqrt_2_pi)
import qualified Statistics.Distribution as D
import qualified Statistics.Sample as S

-- | The normal distribution.
data NormalDistribution = ND {
      mean       :: {-# UNPACK #-} !Double
    , stdDev     :: {-# UNPACK #-} !Double
    , ndPdfDenom :: {-# UNPACK #-} !Double
    , ndCdfDenom :: {-# UNPACK #-} !Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution NormalDistribution where
    cumulative = cumulative

instance D.ContDistr NormalDistribution where
    density    = density
    quantile   = quantile

instance D.Variance NormalDistribution where
    stdDev = stdDev

instance D.Mean NormalDistribution where
    mean = mean

-- | Standard normal distribution with mean equal to 0 and variance equal to 1
standard :: NormalDistribution
standard = ND { mean       = 0.0
              , stdDev     = 1.0
              , ndPdfDenom = m_sqrt_2_pi
              , ndCdfDenom = m_sqrt_2
              }

-- | Create normal distribution from parameters.
--
-- IMPORTANT: prior to 0.10 release second parameter was variance not
-- standard deviation.
normalDistr :: Double            -- ^ Mean of distribution
            -> Double            -- ^ Standard deviation of distribution
            -> NormalDistribution
normalDistr m sd
  | sd > 0    = ND { mean       = m
                   , stdDev     = sd
                   , ndPdfDenom = m_sqrt_2_pi * sd
                   , ndCdfDenom = m_sqrt_2 * sd
                   }
  | otherwise = 
    error $ "Statistics.Distribution.Normal.normalDistr: standard deviation must be positive. Got " ++ show sd

-- | Create distribution using parameters estimated from
--   sample. Variance is estimated using maximum likelihood method
--   (biased estimation).
normalFromSample :: S.Sample -> NormalDistribution
normalFromSample a = normalDistr (S.mean a) (S.stdDev a)

density :: NormalDistribution -> Double -> Double
density d x = exp (-xm * xm / (2 * sd * sd)) / ndPdfDenom d
    where xm = x - mean d
          sd = stdDev d

cumulative :: NormalDistribution -> Double -> Double
cumulative d x = erfc ((mean d - x) / ndCdfDenom d) / 2

quantile :: NormalDistribution -> Double -> Double
quantile d p
  | p < 0 || p > 1 = inf/inf
  | p == 0         = -inf
  | p == 1         = inf
  | p == 0.5       = mean d
  | otherwise      = x * stdDev d + mean d
  where x          = D.findRoot standard p 0 (-100) 100
        inf        = 1/0
