-- |
-- Module    : Statistics.Resampling.Bootstrap
-- Copyright : (c) 2009 Bryan O'Sullivan
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
    ) where

import Data.Array.Vector (foldlU, filterU, indexU, lengthU)
import Statistics.Distribution.Normal hiding (mean)
import Statistics.Distribution (cumulative, inverse)
import Statistics.Resampling (Resample(..), jackknife)
import Statistics.Sample (mean)
import Statistics.Types (Estimator, Sample)

data Estimate = Estimate {
      estPoint           :: {-# UNPACK #-} !Double
    , estLowerBound      :: {-# UNPACK #-} !Double
    , estUpperBound      :: {-# UNPACK #-} !Double
    , estConfidenceLevel :: {-# UNPACK #-} !Double
    } deriving (Eq, Show)

data T = {-# UNPACK #-} !Double :< {-# UNPACK #-} !Double
infixl 2 :<

bootstrapBCA :: Double          -- ^ Confidence level
             -> Sample          -- ^ Sample data
             -> [Estimator]     -- ^ Estimators
             -> [Resample]      -- ^ Resampled data
             -> [Estimate]
bootstrapBCA confidenceLevel sample = zipWith e
  where
    e est (Resample resample) =
        Estimate {
            estPoint = pt
          , estLowerBound = indexU resample lo
          , estUpperBound = indexU resample hi
          , estConfidenceLevel = confidenceLevel
          }
      where
        pt    = est sample
        lo    = max (cumn a1) 0
          where a1 = bias + b1 / (1 - accel * b1)
                b1 = bias + z1
        hi    = min (cumn a2) (ni - 1)
          where a2 = bias + b2 / (1 - accel * b2)
                b2 = bias - z1
        z1    = inverse standard ((1 - confidenceLevel) / 2)
        cumn  = round . (*n) . cumulative standard
        bias  = inverse standard (probN / n)
          where probN = fromIntegral . lengthU . filterU (<pt) $ resample
        ni    = lengthU resample
        n     = fromIntegral ni
        accel = sumCubes / (6 * (sumSquares ** 1.5))
          where (sumSquares :< sumCubes) = foldlU f (0 :< 0) jack
                f (s :< c) j = s + d2 :< c + d2 * d
                    where d  = jackMean - j
                          d2 = d * d
                jackMean     = mean jack
        jack  = jackknife est sample
