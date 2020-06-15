{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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
    , normalDistrE
    , normalDistrErr
    , standard
    ) where

import Control.Applicative
import Data.Aeson            (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary           (Binary(..))
import Data.Data             (Data, Typeable)
import GHC.Generics          (Generic)
import Numeric.MathFunctions.Constants (m_sqrt_2, m_sqrt_2_pi)
import Numeric.SpecFunctions (erfc, invErfc)
import qualified System.Random.MWC.Distributions as MWC
import qualified Data.Vector.Generic as G

import qualified Statistics.Distribution as D
import qualified Statistics.Sample as S
import Statistics.Internal


-- | The normal distribution.
data NormalDistribution = ND {
      mean       :: {-# UNPACK #-} !Double
    , stdDev     :: {-# UNPACK #-} !Double
    , ndPdfDenom :: {-# UNPACK #-} !Double
    , ndCdfDenom :: {-# UNPACK #-} !Double
    } deriving (Eq, Typeable, Data, Generic)

instance Show NormalDistribution where
  showsPrec i (ND m s _ _) = defaultShow2 "normalDistr" m s i
instance Read NormalDistribution where
  readPrec = defaultReadPrecM2 "normalDistr" normalDistrE

instance ToJSON NormalDistribution
instance FromJSON NormalDistribution where
  parseJSON (Object v) = do
    m  <- v .: "mean"
    sd <- v .: "stdDev"
    either fail return $ normalDistrErr m sd
  parseJSON _ = empty

instance Binary NormalDistribution where
    put (ND m sd _ _) = put m >> put sd
    get = do
      m  <- get
      sd <- get
      either fail return $ normalDistrErr m sd

instance D.Distribution NormalDistribution where
    cumulative      = cumulative
    complCumulative = complCumulative

instance D.ContDistr NormalDistribution where
    logDensity    = logDensity
    quantile      = quantile
    complQuantile = complQuantile

instance D.MaybeMean NormalDistribution where
    maybeMean = Just . D.mean

instance D.Mean NormalDistribution where
    mean = mean

instance D.MaybeVariance NormalDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Variance NormalDistribution where
    stdDev = stdDev

instance D.Entropy NormalDistribution where
  entropy d = 0.5 * log (2 * pi * exp 1 * D.variance d)

instance D.MaybeEntropy NormalDistribution where
  maybeEntropy = Just . D.entropy

instance D.ContGen NormalDistribution where
    genContVar d = MWC.normal (mean d) (stdDev d)

-- | Standard normal distribution with mean equal to 0 and variance equal to 1
standard :: NormalDistribution
standard = ND { mean       = 0.0
              , stdDev     = 1.0
              , ndPdfDenom = log m_sqrt_2_pi
              , ndCdfDenom = m_sqrt_2
              }

-- | Create normal distribution from parameters.
--
-- IMPORTANT: prior to 0.10 release second parameter was variance not
-- standard deviation.
normalDistr :: Double            -- ^ Mean of distribution
            -> Double            -- ^ Standard deviation of distribution
            -> NormalDistribution
normalDistr m sd = either error id $ normalDistrErr m sd

-- | Create normal distribution from parameters.
--
-- IMPORTANT: prior to 0.10 release second parameter was variance not
-- standard deviation.
normalDistrE :: Double            -- ^ Mean of distribution
             -> Double            -- ^ Standard deviation of distribution
             -> Maybe NormalDistribution
normalDistrE m sd = either (const Nothing) Just $ normalDistrErr m sd

-- | Create normal distribution from parameters.
--
normalDistrErr :: Double            -- ^ Mean of distribution
               -> Double            -- ^ Standard deviation of distribution
               -> Either String NormalDistribution
normalDistrErr m sd
  | sd > 0    = Right $ ND { mean       = m
                           , stdDev     = sd
                           , ndPdfDenom = log $ m_sqrt_2_pi * sd
                           , ndCdfDenom = m_sqrt_2 * sd
                           }
  | otherwise = Left $ errMsg m sd

errMsg :: Double -> Double -> String
errMsg _ sd = "Statistics.Distribution.Normal.normalDistr: standard deviation must be positive. Got " ++ show sd

-- | Variance is estimated using maximum likelihood method
--   (biased estimation).
--
--   Returns @Nothing@ if sample contains less than one element or
--   variance is zero (all elements are equal)
instance D.FromSample NormalDistribution Double where
  fromSample xs
    | G.length xs <= 1 = Nothing
    | v == 0           = Nothing
    | otherwise        = Just $! normalDistr m (sqrt v)
    where
      (m,v) = S.meanVariance xs

logDensity :: NormalDistribution -> Double -> Double
logDensity d x = (-xm * xm / (2 * sd * sd)) - ndPdfDenom d
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
  where x          = - invErfc (2 * p)
        inf        = 1/0

complQuantile :: NormalDistribution -> Double -> Double
complQuantile d p
  | p == 0         = inf
  | p == 1         = -inf
  | p == 0.5       = mean d
  | p > 0 && p < 1 = x * ndCdfDenom d + mean d
  | otherwise      =
    error $ "Statistics.Distribution.Normal.complQuantile: p must be in [0,1] range. Got: "++show p
  where x          = invErfc (2 * p)
        inf        = 1/0
