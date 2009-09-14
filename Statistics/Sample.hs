{-# LANGUAGE TypeOperators #-}
-- |
-- Module    : Statistics.Sample
-- Copyright : (c) 2008 Don Stewart, 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Commonly used sample statistics, also known as descriptive
-- statistics.

module Statistics.Sample
    (
    -- * Types
      Sample
    -- * Descriptive functions
    , range

    -- * Statistics of location
    , mean
    , harmonicMean
    , geometricMean

    -- * Statistics of dispersion
    -- $variance

    -- ** Central moments
    , centralMoment
    , centralMoments

    -- ** Two-pass functions (numerically robust)
    -- $robust
    , variance
    , varianceUnbiased
    , stdDev

    -- ** Single-pass functions (faster, less safe)
    -- $cancellation
    , fastVariance
    , fastVarianceUnbiased
    , fastStdDev

    -- * References
    -- $references
    ) where

import Data.Array.Vector
import Statistics.Function (minMax)
import Statistics.Types (Sample)

range :: Sample -> Double
range s = hi - lo
    where hi :*: lo = minMax s
{-# INLINE range #-}

-- | Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
mean :: Sample -> Double
mean = fini . foldlU go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
        where m' = m + (x - m) / fromIntegral n'
              n' = n + 1
{-# INLINE mean #-}

-- | Harmonic mean.  This algorithm performs a single pass over the
-- sample.
harmonicMean :: Sample -> Double
harmonicMean = fini . foldlU go (T 0 0)
  where
    fini (T b a) = fromIntegral a / b
    go (T x y) n = T (x + (1/n)) (y+1)
{-# INLINE harmonicMean #-}

-- | Geometric mean of a sample containing no negative values.
geometricMean :: Sample -> Double
geometricMean = fini . foldlU go (T 1 0)
  where
    fini (T p n) = p ** (1 / fromIntegral n)
    go (T p n) a = T (p * a) (n + 1)
{-# INLINE geometricMean #-}

-- | Compute the /k/th central moment of a sample.
centralMoment :: Int -> Sample -> Double
centralMoment a xs
    | a < 0  = error "Statistics.Sample.centralMoment: negative input"
    | a == 0 = 1
    | a == 1 = 0
    | otherwise = sumU (mapU go xs) / fromIntegral (lengthU xs)
  where
    go x = (x-m) ^ a
    m    = mean xs
{-# INLINE centralMoment #-}

-- | Compute two central moments.
centralMoments :: Int -> Int -> Sample -> Double :*: Double
centralMoments a b xs
    | a < 2 || b < 2 = centralMoment a xs :*: centralMoment b xs
    | otherwise      = fini . foldlU go (V 0 0) $ xs
  where go (V i j) x = V (i + d^a) (j + d^a)
            where d  = x - m
        fini (V i j) = i / n :*: j / n
        m            = mean xs
        n            = fromIntegral (lengthU xs)
{-# INLINE centralMoments #-}

-- $variance
--
-- The variance&#8212;and hence the standard deviation&#8212;of a
-- sample of fewer than two elements are both defined to be zero.

-- $robust
--
-- These functions use the compensated summation algorithm of Chan et
-- al. for numerical robustness, but require two passes over the
-- sample data as a result.
--
-- Because of the need for two passes, these functions are /not/
-- subject to stream fusion.

data V = V {-# UNPACK #-} !Double {-# UNPACK #-} !Double

robustVar :: Sample -> T
robustVar samp = fini . foldlU go (V 0 0) $ samp
  where
    go (V s c) x = V (s + d * d) (c + d)
        where d  = x - m
    fini (V s c) = T (s - (c * c) / fromIntegral n) n
    n            = lengthU samp
    m            = mean samp

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance.
variance :: Sample -> Double
variance = fini . robustVar
  where fini (T v n)
          | n > 1     = v / fromIntegral n
          | otherwise = 0
{-# INLINE variance #-}

-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance.
varianceUnbiased :: Sample -> Double
varianceUnbiased = fini . robustVar
  where fini (T v n)
          | n > 1     = v / fromIntegral (n-1)
          | otherwise = 0
{-# INLINE varianceUnbiased #-}

-- | Standard deviation.  This is simply the square root of the
-- maximum likelihood estimate of the variance.
stdDev :: Sample -> Double
stdDev = sqrt . varianceUnbiased

-- $cancellation
--
-- The functions prefixed with the name @fast@ below perform a single
-- pass over the sample data using Knuth's algorithm. They usually
-- work well, but see below for caveats. These functions are subject
-- to array fusion.
--
-- /Note/: in cases where most sample data is close to the sample's
-- mean, Knuth's algorithm gives inaccurate results due to
-- catastrophic cancellation.

fastVar :: Sample -> T1
fastVar = foldlU go (T1 0 0 0)
  where
    go (T1 n m s) x = T1 n' m' s'
      where n' = n + 1
            m' = m + d / fromIntegral n'
            s' = s + d * (x - m')
            d  = x - m

-- | Maximum likelihood estimate of a sample's variance.
fastVariance :: Sample -> Double
fastVariance = fini . fastVar
  where fini (T1 n _m s)
          | n > 1     = s / fromIntegral n
          | otherwise = 0
{-# INLINE fastVariance #-}

-- | Unbiased estimate of a sample's variance.
fastVarianceUnbiased :: Sample -> Double
fastVarianceUnbiased = fini . fastVar
  where fini (T1 n _m s)
          | n > 1     = s / fromIntegral (n - 1)
          | otherwise = 0
{-# INLINE fastVarianceUnbiased #-}

-- | Standard deviation.  This is simply the square root of the
-- maximum likelihood estimate of the variance.
fastStdDev :: Sample -> Double
fastStdDev = sqrt . fastVariance
{-# INLINE fastStdDev #-}

------------------------------------------------------------------------
-- Helper code. Monomorphic unpacked accumulators.

-- don't support polymorphism, as we can't get unboxed returns if we use it.
data T = T {-# UNPACK #-}!Double {-# UNPACK #-}!Int

data T1 = T1 {-# UNPACK #-}!Int {-# UNPACK #-}!Double {-# UNPACK #-}!Double

{-

Consider this core:

with data T a = T !a !Int

$wfold :: Double#
               -> Int#
               -> Int#
               -> (# Double, Int# #)

and without,

$wfold :: Double#
               -> Int#
               -> Int#
               -> (# Double#, Int# #)

yielding to boxed returns and heap checks.

-}

-- $references
--
-- * Chan, T. F.; Golub, G.H.; LeVeque, R.J. (1979) Updating formulae
--   and a pairwise algorithm for computing sample
--   variances. Technical Report STAN-CS-79-773, Department of
--   Computer Science, Stanford
--   University. <ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>
--
-- * Knuth, D.E. (1998) The art of computer programming, volume 2:
--   seminumerical algorithms, 3rd ed., p. 232.
--
-- * Welford, B.P. (1962) Note on a method for calculating corrected
--   sums of squares and products. /Technometrics/
--   4(3):419&#8211;420. <http://www.jstor.org/stable/1266577>
--
-- * West, D.H.D. (1979) Updating mean and variance estimates: an
--   improved method. /Communications of the ACM/
--   22(9):532&#8211;535. <http://doi.acm.org/10.1145/359146.359153>
