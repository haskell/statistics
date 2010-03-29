-- |
-- Module    : Statistics.Autocorrelation
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for computing autocovariance and autocorrelation of a
-- sample.

module Statistics.Autocorrelation
    (
      autocovariance
    , autocorrelation
    ) where

import Statistics.Sample (Sample, mean)
import qualified Data.Vector.Unboxed as U

-- | Compute the autocovariance of a sample, i.e. the covariance of
-- the sample against a shifted version of itself.
autocovariance :: Sample -> U.Vector Double
autocovariance a = U.map f . U.enumFromTo 0 $ l-2
  where
    f k = U.sum (U.zipWith (*) (U.take (l-k) c) (U.slice k (l-k) c))
          / fromIntegral l
    c   = U.map (subtract (mean a)) a
    l   = U.length a

-- | Compute the autocorrelation function of a sample, and the upper
-- and lower bounds of confidence intervals for each element.
--
-- /Note/: The calculation of the 95% confidence interval assumes a
-- stationary Gaussian process.
autocorrelation :: Sample -> (U.Vector Double, U.Vector Double, U.Vector Double)
autocorrelation a = (r, ci (-), ci (+))
  where
    r           = U.map (/ U.head c) c
      where c   = autocovariance a
    dllse       = U.map f . U.scanl1 (+) . U.map square $ r
      where f v = 1.96 * sqrt ((v * 2 + 1) / l)
    l           = fromIntegral (U.length a)
    ci f        = U.cons 1 . U.tail . U.map (f (-1/l)) $ dllse
    square x    = x * x
