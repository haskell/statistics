{-# LANGUAGE FlexibleContexts #-}
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

import Statistics.Sample (mean)
import qualified Data.Vector.Generic as G

-- | Compute the autocovariance of a sample, i.e. the covariance of
-- the sample against a shifted version of itself.
autocovariance :: (G.Vector v Double, G.Vector v Int) => v Double -> v Double
autocovariance a = G.map f . G.enumFromTo 0 $ l-2
  where
    f k = G.sum (G.zipWith (*) (G.take (l-k) c) (G.slice k (l-k) c))
          / fromIntegral l
    c   = G.map (subtract (mean a)) a
    l   = G.length a

-- | Compute the autocorrelation function of a sample, and the upper
-- and lower bounds of confidence intervals for each element.
--
-- /Note/: The calculation of the 95% confidence interval assumes a
-- stationary Gaussian process.
autocorrelation :: (G.Vector v Double, G.Vector v Int) => v Double -> (v Double, v Double, v Double)
autocorrelation a = (r, ci (-), ci (+))
  where
    r           = G.map (/ G.head c) c
      where c   = autocovariance a
    dllse       = G.map f . G.scanl1 (+) . G.map square $ r
      where f v = 1.96 * sqrt ((v * 2 + 1) / l)
    l           = fromIntegral (G.length a)
    ci f        = G.cons 1 . G.tail . G.map (f (-1/l)) $ dllse
    square x    = x * x
