{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TypeFamilies #-}
-- |
-- Module      :  Statistics.Distribution.UMultiNormal
-- Copyright   :  (C) 2012 Doug Beardsley,
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The uncorrelated multivariate normal distribution.  The covariance
-- matrix for this distribution is diagonal.  It can be stored in
-- less space and the density function can be calculated more
-- efficiently than with the more general multivariate normal
-- distribution.

module Statistics.Distribution.UMultiNormal
  ( UMultiNormal
    -- * Constructor
  , uMultiNormal
  ) where

import Prelude hiding (log)
import Data.Number.LogFloat
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric.MathFunctions.Constants
import qualified Statistics.Distribution as D

-- | The uncorrelated multivariate normal distribution.
data UMultiNormal = UMultiNormal
    { mnMean :: Vector Double
    , mnVariance :: Vector Double
    , mnDetSigma :: !Double
    } deriving (Show)

instance D.Distribution UMultiNormal where
    type DistrSample UMultiNormal = Vector Double
    cumulative =
        error "Cumulative distribution function not implemented for multivariate normal"
    complCumulative =
        error "Cumulative distribution function not implemented for multivariate normal"

instance D.ContDistr UMultiNormal where
    density = density
    quantile =
        error "Quantile function not implemented for multivariate normal"
    logDensity = logDensity

instance D.MaybeMean UMultiNormal where
    maybeMean = Just . D.mean

instance D.Mean UMultiNormal where
    mean = mnMean

instance D.MaybeVariance UMultiNormal where
    maybeVariance = Just . mnVariance

instance D.Variance UMultiNormal where
    variance = mnVariance

uMultiNormal :: Vector Double -> Vector Double -> UMultiNormal
uMultiNormal mean variance = UMultiNormal mean variance detSigma
  where
    detSigma = V.product variance

density :: UMultiNormal -> Vector Double -> Double
density UMultiNormal{..} observation =
    exp (-(hph / 2)) / (m_sqrt_2_pi ^ k * sqrt mnDetSigma)
  where
    k = V.length mnMean
    f i = let delta = (observation V.! i) - (mnMean V.! i) in
        delta * delta / (mnVariance V.! i)
    hph = sum $ map f [0..k-1]
{-# INLINE density #-}

logDensity :: UMultiNormal -> Vector Double -> LogFloat
logDensity UMultiNormal{..} observation = logToLogFloat $
    (-(hph / 2)) - (fromIntegral k * m_ln_sqrt_2_pi + log mnDetSigma / 2)
  where
    k = V.length mnMean
    f i = let delta = (observation V.! i) - (mnMean V.! i) in
        delta * delta / (mnVariance V.! i)
    hph = sum $ map f [0..k-1]
{-# INLINE logDensity #-}

