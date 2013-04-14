{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings,
    RecordWildCards #-}

-- |
-- Module    : Statistics.Resampling.Bootstrap
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The bootstrap method for statistical inference.

module Statistics.Resampling.Bootstrap
    (
      Estimate(..)
    , bootstrapBCA
    , scale
    -- * References
    -- $references
    ) where

import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad.Par               (parMap,runPar)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Vector.Unboxed ((!))
import GHC.Generics
import Statistics.Distribution (cumulative, quantile)
import Statistics.Distribution.Normal
import Statistics.Resampling (Resample(..), jackknife)
import Statistics.Sample (mean)
import Statistics.Types (Estimator, Sample)
import qualified Data.Vector.Unboxed as U

-- | A point and interval estimate computed via an 'Estimator'.
data Estimate = Estimate {
      estPoint           :: {-# UNPACK #-} !Double
    -- ^ Point estimate.
    , estLowerBound      :: {-# UNPACK #-} !Double
    -- ^ Lower bound of the estimate interval (i.e. the lower bound of
    -- the confidence interval).
    , estUpperBound      :: {-# UNPACK #-} !Double
    -- ^ Upper bound of the estimate interval (i.e. the upper bound of
    -- the confidence interval).
    , estConfidenceLevel :: {-# UNPACK #-} !Double
    -- ^ Confidence level of the confidence intervals.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary Estimate
instance NFData Estimate

-- | Multiply the point, lower bound, and upper bound in an 'Estimate'
-- by the given value.
scale :: Double                 -- ^ Value to multiply by.
      -> Estimate -> Estimate
scale f e@Estimate{..} = e {
                           estPoint = f * estPoint
                         , estLowerBound = f * estLowerBound
                         , estUpperBound = f * estUpperBound
                         }

estimate :: Double -> Double -> Double -> Double -> Estimate
estimate pt lb ub cl =
    assert (lb <= ub) .
    assert (cl > 0 && cl < 1) $
    Estimate { estPoint = pt
             , estLowerBound = lb
             , estUpperBound = ub
             , estConfidenceLevel = cl
             }

data T = {-# UNPACK #-} !Double :< {-# UNPACK #-} !Double
infixl 2 :<

-- | Bias-corrected accelerated (BCA) bootstrap. This adjusts for both
-- bias and skewness in the resampled distribution.
bootstrapBCA :: Double          -- ^ Confidence level
             -> Sample          -- ^ Sample data
             -> [Estimator]     -- ^ Estimators
             -> [Resample]      -- ^ Resampled data
             -> [Estimate]
bootstrapBCA confidenceLevel sample estimators resamples
  | confidenceLevel > 0 && confidenceLevel < 1
      = runPar $ parMap (uncurry e) (zip estimators resamples)
  | otherwise = error "Statistics.Resampling.Bootstrap.bootstrapBCA: confidence level outside (0,1) range"
  where
    e est (Resample resample)
      | U.length sample == 1 = estimate pt pt pt confidenceLevel
      | otherwise =
          estimate pt (resample ! lo) (resample ! hi) confidenceLevel
      where
        pt    = est sample
        lo    = max (cumn a1) 0
          where a1 = bias + b1 / (1 - accel * b1)
                b1 = bias + z1
        hi    = min (cumn a2) (ni - 1)
          where a2 = bias + b2 / (1 - accel * b2)
                b2 = bias - z1
        z1    = quantile standard ((1 - confidenceLevel) / 2)
        cumn  = round . (*n) . cumulative standard
        bias  = quantile standard (probN / n)
          where probN = fromIntegral . U.length . U.filter (<pt) $ resample
        ni    = U.length resample
        n     = fromIntegral ni
        accel = sumCubes / (6 * (sumSquares ** 1.5))
          where (sumSquares :< sumCubes) = U.foldl' f (0 :< 0) jack
                f (s :< c) j = s + d2 :< c + d2 * d
                    where d  = jackMean - j
                          d2 = d * d
                jackMean     = mean jack
        jack  = jackknife est sample

-- $references
--
-- * Davison, A.C; Hinkley, D.V. (1997) Bootstrap methods and their
--   application. <http://statwww.epfl.ch/davison/BMA/>
