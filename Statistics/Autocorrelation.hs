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

import Data.Array.Vector
import Statistics.Sample (Sample, mean)

-- | Compute the autocovariance of a sample, i.e. the covariance of
-- the sample against a shifted version of itself.
autocovariance :: Sample -> UArr Double
autocovariance a = mapU f . enumFromToU 0 $ l-2
  where
    f k = sumU (zipWithU (*) (takeU (l-k) c) (sliceU c k (l-k)))
          / fromIntegral l
    c   = mapU (subtract (mean a)) a
    l   = lengthU a

-- | Compute the autocorrelation function of a sample, and the upper
-- and lower bounds of confidence intervals for each element.
--
-- /Note/: The calculation of the 95% confidence interval assumes a
-- stationary Gaussian process.
autocorrelation :: Sample -> (UArr Double, UArr Double, UArr Double)
autocorrelation a = (r, ci (-), ci (+))
  where
    r           = mapU (/ headU c) c
      where c   = autocovariance a
    dllse       = mapU f . scanl1U (+) . mapU square $ r
      where f v = 1.96 * sqrt ((v * 2 + 1) / l)
    l           = fromIntegral (lengthU a)
    ci f        = consU 1 . tailU . mapU (f (-1/l)) $ dllse
    square x    = x * x
