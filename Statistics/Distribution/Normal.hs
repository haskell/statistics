{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric #-}
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

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Numeric.MathFunctions.Constants (m_sqrt_2, m_sqrt_2_pi)
import Numeric.SpecFunctions           (erfc, invErfc)
import qualified Statistics.Distribution as D
import qualified Statistics.Sample       as S
import qualified System.Random.MWC.Distributions as MWC



-- | The normal distribution.
data NormalDistribution = ND {
      mean       :: {-# UNPACK #-} !Double
    , stdDev     :: {-# UNPACK #-} !Double
    , ndPdfDenom :: {-# UNPACK #-} !Double
    , ndCdfDenom :: {-# UNPACK #-} !Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary NormalDistribution

instance D.Distribution NormalDistribution where
    cumulative      = cumulative
    complCumulative = complCumulative

instance D.ContDistr NormalDistribution where
    density    = density
    quantile   = quantile

instance D.MaybeMean NormalDistribution where
    maybeMean = Just . D.mean

instance D.Mean NormalDistribution where
    mean = mean

instance D.MaybeVariance NormalDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Variance NormalDistribution where
    stdDev = stdDev

instance D.ContGen NormalDistribution where
    genContVar d = MWC.normal (mean d) (stdDev d)
    {-# INLINE genContVar #-}

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
normalFromSample xs
  = normalDistr m (sqrt v)
  where
    (m,v) = S.meanVariance xs

density :: NormalDistribution -> Double -> Double
density d x = exp (-xm * xm / (2 * sd * sd)) / ndPdfDenom d
    where xm = x - mean d
          sd = stdDev d

cumulative :: NormalDistribution -> Double -> Double
cumulative d x = erfc ((mean d - x) / ndCdfDenom d) / 2

complCumulative :: NormalDistribution -> Double -> Double
complCumulative d x = erfc ((x - mean d) / ndCdfDenom d) / 2

quantile :: NormalDistribution -> Double -> Double
quantile d p
  | p == 0         = -inf
  | p == 1         = inf
  | p == 0.5       = mean d
  | p > 0 && p < 1 = x * ndCdfDenom d + mean d
  | otherwise      =
    error $ "Statistics.Distribution.Normal.quantile: p must be in [0,1] range. Got: "++show p
  where x          = invErfc $ 2 * (1 - p)
        inf        = 1/0
