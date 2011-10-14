{-# LANGUAGE BangPatterns, FlexibleContexts, UnboxedTuples #-}
-- |
-- Module    : Statistics.Sample.KernelDensity
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
--
-- This estimator does not use the commonly employed \"Gaussian rule
-- of thumb\".  As a result, it outperforms many plug-in methods on
-- multimodal samples with widely separated modes.

module Statistics.Sample.KernelDensity
    (
    -- * Estimation functions
      kde
    , kde_
    -- * References
    -- $references
    ) where

import Data.Complex (Complex(..))
import Data.Maybe (fromMaybe)
import Prelude hiding (const,min,max)
import Statistics.Constants (m_sqrt_2_pi)
import Statistics.Function (minMax, nextHighestPowerOfTwo)
import Statistics.Math.RootFinding (ridders)
import Statistics.Sample.Histogram (histogram_)
import Statistics.Transform (dct, idct)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | Gaussian kernel density estimator for one-dimensional data, using
-- the method of Botev et al.
--
-- The result is a pair of vectors, containing:
--
-- * The coordinates of each mesh point.  The mesh interval is chosen
--   to be 20% larger than the range of the sample.  (To specify the
--   mesh interval, use 'kde_'.)
--
-- * Density estimates at each mesh point.
kde :: (G.Vector v Double) =>
       Int
    -- ^ The number of mesh points to use in the uniform discretization
    -- of the interval @(min,max)@.  If this value is not a power of
    -- two, then it is rounded up to the next power of two.
    -> v Double -> (v Double, v Double)
{-# SPECIALIZE kde :: Int -> U.Vector Double
                   -> (U.Vector Double, U.Vector Double) #-}
{-# SPECIALIZE kde :: Int -> V.Vector Double
                   -> (V.Vector Double, V.Vector Double) #-}
kde n0 xs = kde_ n0 (lo - range / 10) (hi + range / 10) xs
  where
    (lo,hi) = minMax xs
    range = hi - lo

-- | Gaussian kernel density estimator for one-dimensional data, using
-- the method of Botev et al.
--
-- The result is a pair of vectors, containing:
--
-- * The coordinates of each mesh point.
--
-- * Density estimates at each mesh point.
kde_ :: (G.Vector v Double) =>
        Int
     -- ^ The number of mesh points to use in the uniform discretization
     -- of the interval @(min,max)@.  If this value is not a power of
     -- two, then it is rounded up to the next power of two.
     -> Double
     -- ^ Lower bound (@min@) of the mesh range.
     -> Double
     -- ^ Upper bound (@max@) of the mesh range.
     -> v Double -> (v Double, v Double)
{-# SPECIALIZE kde_ :: Int -> Double -> Double -> U.Vector Double
                   -> (U.Vector Double, U.Vector Double) #-}
{-# SPECIALIZE kde_ :: Int -> Double -> Double -> V.Vector Double
                   -> (V.Vector Double, V.Vector Double) #-}
kde_ n0 min max xs
  | n0 < 1    = error "Statistics.KernelDensity.kde: invalid number of points"
  | otherwise = (mesh, density)
  where
    mesh = G.generate ni $ \z -> min + (d * fromIntegral z)
        where d = r / (n-1)
    density = G.map (/r) . idct $ G.zipWith f a (G.enumFromTo 0 (n-1))
      where f b z = b * exp (sqr z * sqr pi * t_star * (-0.5)) :+ 0
    !n = fromIntegral ni
    !ni = nextHighestPowerOfTwo n0
    !r = max - min
    a = dct . G.map (/ (G.sum h)) $ h
      where h = G.map (/ (len :+ 0)) $ histogram_ ni min max xs
    !len = fromIntegral (G.length xs)
    !t_star = fromMaybe (0.28 * len ** (-0.4)) . ridders 1e-14 0 0.1 $ \x ->
              x - (len * (2 * sqrt pi) * go 6 (f 7 x)) ** (-0.4)
      where
        f q t = 2 * pi ** (q*2) * G.sum (G.zipWith g iv a2v)
          where g i a2 = i ** q * a2 * exp ((-i) * sqr pi * t)
                a2v = G.map (sqr . (*0.5)) $ G.tail a
                iv = G.map sqr $ G.enumFromTo 1 (n-1)
        go s !h | s == 1    = h
                | otherwise = go (s-1) (f s time)
          where time = (2 * const * k0 / len / h) ** (2 / (3 + 2 * s))
                const = (1 + 0.5 ** (s+0.5)) / 3
                k0 = U.product (G.enumFromThenTo 1 3 (2*s-1)) / m_sqrt_2_pi
    _bandwidth = sqrt t_star * r
    sqr x = x * x

-- $references
--
-- Botev. Z.I., Grotowski J.F., Kroese D.P. (2010). Kernel density
-- estimation via diffusion. /Annals of Statistics/
-- 38(5):2916&#8211;2957. <http://arxiv.org/pdf/1011.2602>
