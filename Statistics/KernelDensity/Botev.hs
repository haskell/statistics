{-# LANGUAGE BangPatterns, FlexibleContexts, UnboxedTuples #-}
-- |
-- Module    : Statistics.KernelDensity.Botev
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Kernel density estimation.  This module provides a fast, robust,
-- non-parametric way to estimate the probability density function of
-- a sample.

module Statistics.KernelDensity.Botev
    (
      kde
    -- * References
    -- $references
    ) where

import Data.Complex (Complex(..))
import Data.Maybe (fromMaybe)
import Prelude hiding (const,min,max)
import Statistics.Function (minMax, nextHighestPowerOfTwo)
import Statistics.Histogram (histogram_)
import Statistics.RootFinding (ridders)
import Statistics.Transform (dct, idct)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

kde :: (G.Vector v Double) => Int -> v Double -> (v Double, v Double)
{-# SPECIALIZE kde :: Int -> U.Vector Double
                   -> (U.Vector Double, U.Vector Double) #-}
{-# SPECIALIZE kde :: Int -> V.Vector Double
                   -> (V.Vector Double, V.Vector Double) #-}
kde n0 xs = (mesh, density)
  where
    mesh = G.generate ni $ \z -> min + (d * fromIntegral z)
        where d = r / (n-1)
    density = G.map (/r) . idct $ G.zipWith f a (G.enumFromTo 0 (n-1))
      where f b z = b * exp (sqr (-z) * sqr pi * t_star * 0.5) :+ 0
    !n = fromIntegral ni
    !ni = nextHighestPowerOfTwo n0
    (lo,hi) = minMax xs
    (# !min, !max #) = (# lo - range / 10, hi + range / 10 #)
      where range = hi - lo
    !r = max - min
    a = dct . G.map (/ (G.sum h * fromIntegral (G.length xs))) $ h
      where h = histogram_ ni min max xs
    !t_star = fromMaybe (0.28 * n ** (-0.4)) $ ridders 1e-14 fixed_point 0 0.1
    fixed_point x = x - (2 * n * sqrt pi * go 6 (f 7 x)) ** (-0.4)
      where
        f q t = 2 * pi ** (q*2) * G.sum (G.zipWith g z a2)
          where g k j = k ** q * j * exp ((-k) * sqr pi * t)
                a2 = G.map (sqr . (*0.5)) $ G.tail a
                z = G.map sqr $ G.enumFromTo 1 (n-1)
        go s !h | s == 1    = h
                | otherwise = go (s-1) (f s time)
          where time = (2 * const * k0 / n / h) ** (2 / (3 + 2 * s))
                const = (1 + 0.5 ** (s+0.5)) / 3
                k0 = U.product (G.enumFromThenTo 1 3 (2*s-1)) / sqrt (2*pi)
    _bandwidth = sqrt t_star * r
    sqr x = x * x

-- $references
--
-- Botev. Z.I., Grotowski J.F., Kroese D.P. (2010). Kernel density
-- estimation via diffusion. /Annals of Statistics/
-- 38(5):2916&#8211;2957. <http://arxiv.org/pdf/1011.2602>
