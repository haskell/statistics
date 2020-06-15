{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Lognormal
-- Copyright : (c) 2020 Ximin Luo
-- License   : BSD3
--
-- Maintainer  : infinity0@pwned.gg
-- Stability   : experimental
-- Portability : portable
--
-- The log normal distribution.  This is a continuous probability
-- distribution that describes data whose log is clustered around a
-- mean. For example, the multiplicative product of many independent
-- positive random variables.

module Statistics.Distribution.Lognormal
    (
      LognormalDistribution
      -- * Constructors
    , lognormalDistr
    , lognormalDistrE
    , lognormalStandard
    ) where

import Data.Binary           (Binary (..))
import Data.Data             (Data, Typeable)
import Data.Maybe            (fromMaybe)
import GHC.Generics          (Generic)
import Numeric.MathFunctions.Constants (m_sqrt_2_pi)
import qualified Data.Vector.Generic as G

import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Normal as N
import Statistics.Internal


-- | The lognormal distribution.
newtype LognormalDistribution = LND N.NormalDistribution
    deriving (Eq, Typeable, Data, Generic)

instance Show LognormalDistribution where
  showsPrec i (LND d) = defaultShow2 "lognormalDistr" m s i
   where
    m = D.mean d
    s = D.stdDev d
instance Read LognormalDistribution where
  readPrec = defaultReadPrecM2 "lognormalDistr" lognormalDistrE

instance Binary LognormalDistribution where
  put (LND d) = put m >> put s
   where
    m = D.mean d
    s = D.stdDev d
  get = do
    m  <- get
    sd <- get
    maybe (fail $ errMsg m sd) return $ lognormalDistrE m sd

instance D.Distribution LognormalDistribution where
  cumulative      = cumulative
  complCumulative = complCumulative

instance D.ContDistr LognormalDistribution where
  logDensity    = logDensity
  quantile      = quantile
  complQuantile = complQuantile

instance D.MaybeMean LognormalDistribution where
  maybeMean = Just . D.mean

instance D.Mean LognormalDistribution where
  mean (LND d) = exp (m + v / 2)
   where
    m = D.mean d
    v = D.variance d

instance D.MaybeVariance LognormalDistribution where
  maybeStdDev   = Just . D.stdDev
  maybeVariance = Just . D.variance

instance D.Variance LognormalDistribution where
  variance (LND d) = (exp v - 1) * exp (2 * m + v)
   where
    m = D.mean d
    v = D.variance d

instance D.Entropy LognormalDistribution where
  entropy (LND d) = logBase 2 (s * exp (m + 0.5) * m_sqrt_2_pi)
   where
    m = D.mean d
    s = D.stdDev d

instance D.MaybeEntropy LognormalDistribution where
  maybeEntropy = Just . D.entropy

instance D.ContGen LognormalDistribution where
  genContVar d = D.genContinuous d

-- | Standard log normal distribution with mu 0 and sigma 1.
--
-- Mean is @sqrt e@ and variance is @(e - 1) * e@.
lognormalStandard :: LognormalDistribution
lognormalStandard = LND N.standard

-- | Create log normal distribution from parameters.
lognormalDistr
  :: Double            -- ^ Mu
  -> Double            -- ^ Sigma
  -> LognormalDistribution
lognormalDistr m sd = fromMaybe (error $ errMsg m sd) $ lognormalDistrE m sd

-- | Create log normal distribution from parameters.
lognormalDistrE
  :: Double            -- ^ Mu
  -> Double            -- ^ Sigma
  -> Maybe LognormalDistribution
lognormalDistrE m sd = LND <$> N.normalDistrE m sd

errMsg :: Double -> Double -> String
errMsg _ sd =
  "Statistics.Distribution.Lognormal.lognormalDistr: sigma must be positive. Got "
    ++ show sd

-- | Variance is estimated using maximum likelihood method
--   (biased estimation) over the log of the data.
--
--   Returns @Nothing@ if sample contains less than one element or
--   variance is zero (all elements are equal)
instance D.FromSample LognormalDistribution Double where
  fromSample = fmap LND . D.fromSample . G.map log

logDensity :: LognormalDistribution -> Double -> Double
logDensity (LND d) x = D.logDensity d lx - lx where lx = log x

cumulative :: LognormalDistribution -> Double -> Double
cumulative (LND d) = D.cumulative d . log

complCumulative :: LognormalDistribution -> Double -> Double
complCumulative (LND d) = D.complCumulative d . log

quantile :: LognormalDistribution -> Double -> Double
quantile (LND d) = exp . D.quantile d

complQuantile :: LognormalDistribution -> Double -> Double
complQuantile (LND d) = exp . D.complQuantile d
