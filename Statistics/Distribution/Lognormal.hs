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
    , lognormalDistrErr
    , lognormalDistrMeanStddevErr
    , lognormalStandard
    ) where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Binary           (Binary (..))
import Data.Data             (Data, Typeable)
import GHC.Generics          (Generic)
import Numeric.MathFunctions.Constants (m_huge, m_sqrt_2_pi)
import Numeric.SpecFunctions (expm1, log1p)
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
  readPrec = defaultReadPrecM2 "lognormalDistr" $
    (either (const Nothing) Just .) . lognormalDistrErr

instance ToJSON LognormalDistribution
instance FromJSON LognormalDistribution

instance Binary LognormalDistribution where
  put (LND d) = put m >> put s
   where
    m = D.mean d
    s = D.stdDev d
  get = do
    m  <- get
    sd <- get
    either fail return $ lognormalDistrErr m sd

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
  variance (LND d) = expm1 v * exp (2 * m + v)
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
lognormalDistr mu sig = either error id $ lognormalDistrErr mu sig

-- | Create log normal distribution from parameters.
lognormalDistrErr
  :: Double            -- ^ Mu
  -> Double            -- ^ Sigma
  -> Either String LognormalDistribution
lognormalDistrErr mu sig
  | sig >= sqrt (log m_huge - 2 * mu) = Left $ errMsg mu sig
  | otherwise = LND <$> N.normalDistrErr mu sig

errMsg :: Double -> Double -> String
errMsg mu sig =
  "Statistics.Distribution.Lognormal.lognormalDistr: sigma must be > 0 && < "
    ++ show lim ++ ". Got " ++ show sig
  where lim = sqrt (log m_huge - 2 * mu)

-- | Create log normal distribution from mean and standard deviation.
lognormalDistrMeanStddevErr
  :: Double            -- ^ Mu
  -> Double            -- ^ Sigma
  -> Either String LognormalDistribution
lognormalDistrMeanStddevErr m sd = LND <$> N.normalDistrErr mu sig
  where r = sd / m
        sig2 = log1p (r * r)
        sig = sqrt sig2
        mu = log m - sig2 / 2

-- | Variance is estimated using maximum likelihood method
--   (biased estimation) over the log of the data.
--
--   Returns @Nothing@ if sample contains less than one element or
--   variance is zero (all elements are equal)
instance D.FromSample LognormalDistribution Double where
  fromSample = fmap LND . D.fromSample . G.map log

logDensity :: LognormalDistribution -> Double -> Double
logDensity (LND d) x
  | x > 0 = let lx = log x in D.logDensity d lx - lx
  | otherwise = 0

cumulative :: LognormalDistribution -> Double -> Double
cumulative (LND d) x
  | x > 0 = D.cumulative d $ log x
  | otherwise = 0

complCumulative :: LognormalDistribution -> Double -> Double
complCumulative (LND d) x
  | x > 0 = D.complCumulative d $ log x
  | otherwise = 1

quantile :: LognormalDistribution -> Double -> Double
quantile (LND d) = exp . D.quantile d

complQuantile :: LognormalDistribution -> Double -> Double
complQuantile (LND d) = exp . D.complQuantile d
