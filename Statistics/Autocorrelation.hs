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

import qualified Data.Vector.Generic as G
import Prelude hiding (sum)

import Statistics.Function        (square)
import Statistics.Sample          (mean)
import Statistics.Sample.Internal (sum)
import Statistics.Types           (partial)


-- | Compute the autocovariance of a sample, i.e. the covariance of
--   the sample against a shifted version of itself:
--
--   \[
--   \operatorname{cov}(X_i, X_{i+k})
--   \]
--
--   Function returns array of covariances where i'th element
--   corresponds to autocovariance with offset @i@. Note that
--   autocovariance at offset 0 coincides with variance of sample. If
--   sample contains less that 2 elements empty vector is returned.
autocovariance
  :: (G.Vector v Double, G.Vector v Int)
  => v Double -> v Double
autocovariance xs
  = G.generate (n - 2) auto
  where
    auto k = sum (G.zipWith (*) (G.take (n-k) xs') (G.drop k xs'))
           / fromIntegral n
    xs'    = G.map (subtract mu) xs
    -- SAFE: It's never called if sample size if too small
    mu     = partial $ mean xs
    n      = G.length xs


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
