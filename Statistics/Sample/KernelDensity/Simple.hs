{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}
-- |
-- Module    : Statistics.Sample.KernelDensity.Simple
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Kernel density estimation code, providing non-parametric ways to
-- estimate the probability density function of a sample.
--
-- The techniques used by functions in this module are relatively
-- fast, but they generally give inferior results to the KDE function
-- in the main 'Statistics.KernelDensity' module (due to the
-- oversmoothing documented for 'bandwidth' below).

module Statistics.Sample.KernelDensity.Simple
    {-# DEPRECATED "Use Statistics.Sample.KernelDensity instead." #-}
    (
    -- * Simple entry points
      epanechnikovPDF
    , gaussianPDF
    -- * Building blocks
    -- These functions may be useful if you need to construct a kernel
    -- density function estimator other than the ones provided in this
    -- module.

    -- ** Choosing points from a sample
    , Points(..)
    , choosePoints
    -- ** Bandwidth estimation
    , Bandwidth
    , bandwidth
    , epanechnikovBW
    , gaussianBW
    -- ** Kernels
    , Kernel
    , epanechnikovKernel
    , gaussianKernel
    -- ** Low-level estimation
    , estimatePDF
    , simplePDF
    -- * References
    -- $references
    ) where

import Data.Binary (Binary(..))
import Data.Data (Data, Typeable)
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import Numeric.MathFunctions.Constants (m_1_sqrt_2, m_2_sqrt_pi)
import Prelude hiding (sum)
import Statistics.Function (minMax)
import Statistics.Sample (stdDev)
import Statistics.Sample.Internal (sum)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | Points from the range of a 'Sample'.
newtype Points = Points {
      fromPoints :: U.Vector Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary Points where
    get = fmap Points get
    put = put . fromPoints

-- | Bandwidth estimator for an Epanechnikov kernel.
epanechnikovBW :: Double -> Bandwidth
epanechnikovBW n = (80 / (n * m_2_sqrt_pi)) ** 0.2

-- | Bandwidth estimator for a Gaussian kernel.
gaussianBW :: Double -> Bandwidth
gaussianBW n = (4 / (n * 3)) ** 0.2

-- | The width of the convolution kernel used.
type Bandwidth = Double

-- | Compute the optimal bandwidth from the observed data for the
-- given kernel.
--
-- This function uses an estimate based on the standard deviation of a
-- sample (due to Deheuvels), which performs reasonably well for
-- unimodal distributions but leads to oversmoothing for more complex
-- ones.
bandwidth :: G.Vector v Double =>
             (Double -> Bandwidth)
          -> v Double
          -> Bandwidth
bandwidth kern values = stdDev values * kern (fromIntegral $ G.length values)

-- | Choose a uniform range of points at which to estimate a sample's
-- probability density function.
--
-- If you are using a Gaussian kernel, multiply the sample's bandwidth
-- by 3 before passing it to this function.
--
-- If this function is passed an empty vector, it returns values of
-- positive and negative infinity.
choosePoints :: G.Vector v Double =>
                Int             -- ^ Number of points to select, /n/
             -> Double          -- ^ Sample bandwidth, /h/
             -> v Double        -- ^ Input data
             -> Points
choosePoints n h sample = Points . U.map f $ U.enumFromTo 0 n'
  where lo     = a - h
        hi     = z + h
        (a, z) = minMax sample
        d      = (hi - lo) / fromIntegral n'
        f i    = lo + fromIntegral i * d
        n'     = n - 1

-- | The convolution kernel.  Its parameters are as follows:
--
-- * Scaling factor, 1\//nh/
--
-- * Bandwidth, /h/
--
-- * A point at which to sample the input, /p/
--
-- * One sample value, /v/
type Kernel =  Double
            -> Double
            -> Double
            -> Double
            -> Double

-- | Epanechnikov kernel for probability density function estimation.
epanechnikovKernel :: Kernel
epanechnikovKernel f h p v
    | abs u <= 1 = f * (1 - u * u)
    | otherwise  = 0
    where u = (v - p) / (h * 0.75)

-- | Gaussian kernel for probability density function estimation.
gaussianKernel :: Kernel
gaussianKernel f h p v = exp (-0.5 * u * u) * g
    where u = (v - p) / h
          g = f * 0.5 * m_2_sqrt_pi * m_1_sqrt_2

-- | Kernel density estimator, providing a non-parametric way of
-- estimating the PDF of a random variable.
estimatePDF :: G.Vector v Double =>
               Kernel           -- ^ Kernel function
            -> Bandwidth        -- ^ Bandwidth, /h/
            -> v Double         -- ^ Sample data
            -> Points           -- ^ Points at which to estimate
            -> U.Vector Double
estimatePDF kernel h sample
    | n < 2     = errorShort "estimatePDF"
    | otherwise = U.map k . fromPoints
  where
    k p = sum . G.map (kernel f h p) $ sample
    f   = 1 / (h * fromIntegral n)
    n   = G.length sample
{-# INLINE estimatePDF #-}

-- | A helper for creating a simple kernel density estimation function
-- with automatically chosen bandwidth and estimation points.
simplePDF :: G.Vector v Double =>
             (Double -> Double) -- ^ Bandwidth function
          -> Kernel             -- ^ Kernel function
          -> Double             -- ^ Bandwidth scaling factor (3 for a Gaussian kernel, 1 for all others)
          -> Int                -- ^ Number of points at which to estimate
          -> v Double           -- ^ sample data
          -> (Points, U.Vector Double)
simplePDF fbw fpdf k numPoints sample =
    (points, estimatePDF fpdf bw sample points)
  where points = choosePoints numPoints (bw*k) sample
        bw     = bandwidth fbw sample
{-# INLINE simplePDF #-}

-- | Simple Epanechnikov kernel density estimator.  Returns the
-- uniformly spaced points from the sample range at which the density
-- function was estimated, and the estimates at those points.
epanechnikovPDF :: G.Vector v Double =>
                   Int          -- ^ Number of points at which to estimate
                -> v Double     -- ^ Data sample
                -> (Points, U.Vector Double)
epanechnikovPDF = simplePDF epanechnikovBW epanechnikovKernel 1

-- | Simple Gaussian kernel density estimator.  Returns the uniformly
-- spaced points from the sample range at which the density function
-- was estimated, and the estimates at those points.
gaussianPDF :: G.Vector v Double =>
               Int              -- ^ Number of points at which to estimate
            -> v Double         -- ^ Data sample
            -> (Points, U.Vector Double)
gaussianPDF = simplePDF gaussianBW gaussianKernel 3

errorShort :: String -> a
errorShort func = error ("Statistics.KernelDensity." ++ func ++
                        ": at least two points required")

-- $references
--
-- * Deheuvels, P. (1977) Estimation non paramétrique de la densité
--   par histogrammes
--   généralisés. Mhttp://archive.numdam.org/article/RSA_1977__25_3_5_0.pdf>
