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

import Data.Default.Class
import Numeric.MathFunctions.Constants (m_sqrt_2_pi)
import Numeric.RootFinding             (fromRoot, ridders, RiddersParam(..), Tolerance(..))
import Prelude hiding (const, min, max, sum)
import Statistics.Function (minMax, nextHighestPowerOfTwo)
import Statistics.Sample.Histogram (histogram_)
import Statistics.Sample.Internal (sum)
import Statistics.Transform (CD, dct, idct)
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector          as V


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
kde :: (G.Vector v CD, G.Vector v Double, G.Vector v Int)
    => Int
    -- ^ The number of mesh points to use in the uniform discretization
    -- of the interval @(min,max)@.  If this value is not a power of
    -- two, then it is rounded up to the next power of two.
    -> v Double -> (v Double, v Double)
kde n0 xs = kde_ n0 (lo - range / 10) (hi + range / 10) xs
  where
    (lo,hi) = minMax xs
    range   | G.length xs <= 1 = 1       -- Unreasonable guess
            | lo == hi         = 1       -- All elements are equal
            | otherwise        = hi - lo
{-# INLINABLE  kde #-}
{-# SPECIAlIZE kde :: Int -> U.Vector Double -> (U.Vector Double, U.Vector Double) #-}
{-# SPECIAlIZE kde :: Int -> V.Vector Double -> (V.Vector Double, V.Vector Double) #-}


-- | Gaussian kernel density estimator for one-dimensional data, using
-- the method of Botev et al.
--
-- The result is a pair of vectors, containing:
--
-- * The coordinates of each mesh point.
--
-- * Density estimates at each mesh point.
kde_ :: (G.Vector v CD, G.Vector v Double, G.Vector v Int)
     => Int
     -- ^ The number of mesh points to use in the uniform discretization
     -- of the interval @(min,max)@.  If this value is not a power of
     -- two, then it is rounded up to the next power of two.
     -> Double
     -- ^ Lower bound (@min@) of the mesh range.
     -> Double
     -- ^ Upper bound (@max@) of the mesh range.
     -> v Double
     -> (v Double, v Double)
kde_ n0 min max xs
  | G.null xs = error "Statistics.KernelDensity.kde: empty sample"
  | n0 <= 1   = error "Statistics.KernelDensity.kde: invalid number of points"
  | otherwise = (mesh, density)
  where
    mesh = G.generate ni $ \z -> min + (d * fromIntegral z)
        where d = r / (n-1)
    density = G.map (/(2 * r)) . idct $ G.zipWith f a (G.enumFromTo 0 (n-1))
      where f b z = b * exp (sqr z * sqr pi * t_star * (-0.5))
    !n  = fromIntegral ni
    !ni = nextHighestPowerOfTwo n0
    !r  = max - min
    a   = dct . G.map (/ sum h) $ h
        where h = G.map (/ len) $ histogram_ ni min max xs
    !len    = fromIntegral (G.length xs)
    !t_star = fromRoot (0.28 * len ** (-0.4)) . ridders def{ riddersTol = AbsTol 1e-14 } (0,0.1)
            $ \x -> x - (len * (2 * sqrt pi) * go 6 (f 7 x)) ** (-0.4)
      where
        f q t = 2 * pi ** (q*2) * sum (G.zipWith g iv a2v)
          where g i a2 = i ** q * a2 * exp ((-i) * sqr pi * t)
                a2v = G.map (sqr . (*0.5)) $ G.tail a
                iv = G.map sqr $ G.enumFromTo 1 (n-1)
        go s !h | s == 1    = h
                | otherwise = go (s-1) (f s time)
          where time  = (2 * const * k0 / len / h) ** (2 / (3 + 2 * s))
                const = (1 + 0.5 ** (s+0.5)) / 3
                k0    = U.product (G.enumFromThenTo 1 3 (2*s-1)) / m_sqrt_2_pi
    sqr x = x * x
{-# INLINABLE  kde_ #-}
{-# SPECIAlIZE kde_ :: Int -> Double -> Double -> U.Vector Double -> (U.Vector Double, U.Vector Double) #-}
{-# SPECIAlIZE kde_ :: Int -> Double -> Double -> V.Vector Double -> (V.Vector Double, V.Vector Double) #-}


-- $references
--
-- Botev. Z.I., Grotowski J.F., Kroese D.P. (2010). Kernel density
-- estimation via diffusion. /Annals of Statistics/
-- 38(5):2916&#8211;2957. <http://arxiv.org/pdf/1011.2602>
