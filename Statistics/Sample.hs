{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , WeightedSample
    -- * Descriptive functions
    , range

    -- * Statistics of location
    , mean
    , meanWeighted
    , harmonicMean
    , geometricMean

    -- * Statistics of dispersion
    -- $variance

    -- ** Two-pass functions (numerically robust)
    -- $robust
    , variance
    , varianceML
    , stdDev
    , stdDevML
    , stdErrMean

    -- ** Functions over central moments
    , centralMoment
    , centralMoments
    , skewness
    , kurtosis

    -- * Joint distirbutions
    , covariance
    , correlation
    , pair
    -- * References
    -- $references
    ) where

import Control.Monad       (liftM)
import Control.Monad.Catch (MonadThrow(..))
import Statistics.Function (minMax)
import Statistics.Sample.Internal (robustSumVar, sum)
import Statistics.Types.Internal  (Sample,WeightedSample,StatisticsException(..))
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S
-- Operator ^ will be overridden
import Prelude hiding ((^), sum)

-- | /O(n)/ Range. The difference between the largest and smallest
-- elements of a sample.
range :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
range xs
  | G.null xs = modErr "range" "Empty sample"
  | otherwise = return $! let (lo , hi) = minMax xs in hi - lo
{-# INLINE range #-}

-- | /O(n)/ Arithmetic mean.  This uses Kahan-Babuška-Neumaier
--   summation.
mean :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
mean xs
  | G.null xs = modErr "mean" "Empty sample"
  | otherwise = return $! sum xs / fromIntegral (G.length xs)
{-# SPECIALIZE mean :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE mean :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE mean :: MonadThrow m => S.Vector Double -> m Double #-}

-- | /O(n)/ Arithmetic mean for weighted sample. It uses a single-pass
-- algorithm analogous to the one used by 'welfordMean'.
meanWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> Double
meanWeighted = fini . G.foldl' go (V 0 0)
    where
      fini (V a _) = a
      go (V m w) (x,xw) = V m' w'
          where m' | w' == 0   = 0
                   | otherwise = m + xw * (x - m) / w'
                w' = w + xw
{-# INLINE meanWeighted #-}

-- | /O(n)/ Harmonic mean.
harmonicMean :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
harmonicMean xs = do
  m <- mean $ G.map recip xs
  return $! fromIntegral (G.length xs) / m
{-# INLINE harmonicMean #-}

-- | /O(n)/ Geometric mean of a sample containing no negative values.
geometricMean :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
geometricMean = liftM exp . mean . G.map log
{-# INLINE geometricMean #-}

-- | Compute the /k/th central moment of a sample.  The central moment
-- is also known as the moment about the mean.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMoment
  :: (G.Vector v Double, MonadThrow m)
  => Int      -- ^ Central moment to compute. Must be nonnegative
  -> v Double -- ^ Data sample
  -> m Double
centralMoment a xs
  | a < 0     = modErr "centralMoment" "Negative central moment"
  | a == 0    = return 1
  | a == 1    = return 0
  | otherwise = do m <- mean xs
                   let cmoment x = (x - m) ^ a
                   return $! sum (G.map cmoment xs) / fromIntegral (G.length xs)
{-# SPECIALIZE centralMoment :: MonadThrow m => Int -> V.Vector Double -> m Double #-}
{-# SPECIALIZE centralMoment :: MonadThrow m => Int -> U.Vector Double -> m Double #-}
{-# SPECIALIZE centralMoment :: MonadThrow m => Int -> S.Vector Double -> m Double #-}

-- | Compute the /k/th and /j/th central moments of a sample.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMoments :: (G.Vector v Double, MonadThrow m) => Int -> Int -> v Double -> m (Double, Double)
centralMoments a b xs
  | a < 2 || b < 2 = do !cA <- centralMoment a xs
                        !cB <- centralMoment b xs
                        return (cA,cB)
  | otherwise      = do m <- mean xs
                        let step (V i j) x = V (i + d^a) (j + d^b)
                              where d = x - m
                            fini (V i j) = (i / n , j / n)
                        return $! fini $ G.foldl' step (V 0 0) xs
  where
    n = fromIntegral (G.length xs)
{-# SPECIALIZE centralMoments
      :: MonadThrow m => Int -> Int -> V.Vector Double -> m (Double, Double) #-}
{-# SPECIALIZE centralMoments
      :: MonadThrow m => Int -> Int -> U.Vector Double -> m (Double, Double) #-}
{-# SPECIALIZE centralMoments
      :: MonadThrow m => Int -> Int -> S.Vector Double -> m (Double, Double) #-}



-- | Compute the skewness of a sample. This is a measure of the
-- asymmetry of its distribution.
--
-- A sample with negative skew is said to be /left-skewed/.  Most of
-- its mass is on the right of the distribution, with the tail on the
-- left.
--
-- > skewness $ U.to [1,100,101,102,103]
-- > ==> -1.497681449918257
--
-- A sample with positive skew is said to be /right-skewed/.
--
-- > skewness $ U.to [1,2,3,4,100]
-- > ==> 1.4975367033335198
--
-- A sample's skewness is not defined if its 'variance' is zero.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
skewness :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
skewness xs = do
  (c3 , c2) <- centralMoments 3 2 xs
  return $! c3 * c2 ** (-1.5)
{-# SPECIALIZE skewness :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE skewness :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE skewness :: MonadThrow m => S.Vector Double -> m Double #-}


-- | Compute the excess kurtosis of a sample.  This is a measure of
-- the \"peakedness\" of its distribution.  A high kurtosis indicates
-- that more of the sample's variance is due to infrequent severe
-- deviations, rather than more frequent modest deviations.
--
-- A sample's excess kurtosis is not defined if its 'variance' is
-- zero.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
kurtosis :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
kurtosis xs = do
  (c4 , c2) <- centralMoments 4 2 xs
  return $! c4 / (c2 * c2) - 3
{-# SPECIALIZE kurtosis :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE kurtosis :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE kurtosis :: MonadThrow m => S.Vector Double -> m Double #-}

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

-- | Unbiased estimate of a sample's variance. Also known as the
--   sample variance
--
--   \[ \sigma^2 = \frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 \]
variance :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
variance xs
  | n > 2     = do m <- mean xs
                   return $! robustSumVar m xs / fromIntegral (n - 1)
  | otherwise = modErr "variance" "Insufficient sample size"
  where
    n = G.length xs
{-# SPECIALIZE variance :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE variance :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE variance :: MonadThrow m => S.Vector Double -> m Double #-}

-- | Maximum likelihood estimate of a sample's variance. Also known
--   as the population variance:
--
--   \[ \sigma^2 = \frac{1}{N}\sum_{i=1}^N(x_i - \bar{x})^2 \]
varianceML :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
varianceML xs
  | n > 1     = do m <- mean xs
                   return $! robustSumVar m xs / fromIntegral n
  | otherwise = modErr "varianceML" "Insufficient sample size"
  where
    n = G.length xs
{-# SPECIALIZE varianceML :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE varianceML :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE varianceML :: MonadThrow m => S.Vector Double -> m Double #-}

-- | Standard deviation. This is simply the square root of the
--   unbiased estimate of the variance. Note that this estimate is not
--   unbiased itself.
--
--   \[ \sigma = \sqrt{\frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 } \]
stdDev :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
stdDev = liftM sqrt . variance
{-# SPECIALIZE stdDev :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE stdDev :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE stdDev :: MonadThrow m => S.Vector Double -> m Double #-}

-- | Standard deviation. This is simply the square root of the
--   maximum likelihood estimate of the variance.
--
--   \[ \sigma = \sqrt{\frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 } \]
stdDevML :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
stdDevML = liftM sqrt . varianceML
{-# SPECIALIZE stdDev :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE stdDev :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE stdDev :: MonadThrow m => S.Vector Double -> m Double #-}

-- | Standard error of the mean. This is the standard deviation
--   divided by the square root of the sample size.
stdErrMean :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
stdErrMean xs = do
  s <- stdDev xs
  return $! s / (sqrt . fromIntegral . G.length) xs
{-# SPECIALIZE stdErrMean :: MonadThrow m => V.Vector Double -> m Double #-}
{-# SPECIALIZE stdErrMean :: MonadThrow m => U.Vector Double -> m Double #-}
{-# SPECIALIZE stdErrMean :: MonadThrow m => S.Vector Double -> m Double #-}

-- data SampleVariance = SampleVariance
--   { sampleSize  :: !Int
--   , sampleMean  :: !Double
--   , sampleSumSq :: !Double
--   }

-- meanVariance :: (G.Vector v Double) => v Double -> SampleVariance
-- meanVariance xs = SampleVariance
--   { sampleSize  = n
--   , sampleMean  = m
--   , sampleSumSq = robustSumVar m xs
--   }
--   where
--     n             = G.length xs
--     m | n == 0    = 0
--       | otherwise = sum xs / fromIntegral n
-- {-# SPECIALIZE meanVariance :: V.Vector Double -> SampleVariance #-}
-- {-# SPECIALIZE meanVariance :: U.Vector Double -> SampleVariance #-}
-- {-# SPECIALIZE meanVariance :: S.Vector Double -> SampleVariance #-}

-- -- | Calculate mean and maximum likelihood estimate of variance. This
-- -- function should be used if both mean and variance are required
-- -- since it will calculate mean only once.
-- meanVariance ::  (G.Vector v Double) => v Double -> (Double,Double)
-- meanVariance samp
--   | n > 1     = (m, robustSumVar m samp / fromIntegral n)
--   | otherwise = (m, 0)
--     where
--       n = G.length samp
--       m = mean samp
-- {-# SPECIALIZE meanVariance :: U.Vector Double -> (Double,Double) #-}
-- {-# SPECIALIZE meanVariance :: V.Vector Double -> (Double,Double) #-}

-- -- | Calculate mean and unbiased estimate of variance. This
-- -- function should be used if both mean and variance are required
-- -- since it will calculate mean only once.
-- meanVarianceUnb :: (G.Vector v Double) => v Double -> (Double,Double)
-- meanVarianceUnb samp
--   | n > 1     = (m, robustSumVar m samp / fromIntegral (n-1))
--   | otherwise = (m, 0)
--     where
--       n = G.length samp
--       m = mean samp
-- {-# SPECIALIZE meanVarianceUnb :: U.Vector Double -> (Double,Double) #-}
-- {-# SPECIALIZE meanVarianceUnb :: V.Vector Double -> (Double,Double) #-}

-- -- | Standard deviation.  This is simply the square root of the
-- -- unbiased estimate of the variance.
-- stdDev :: (G.Vector v Double) => v Double -> Double
-- stdDev = sqrt . varianceUnbiased
-- {-# SPECIALIZE stdDev :: U.Vector Double -> Double #-}
-- {-# SPECIALIZE stdDev :: V.Vector Double -> Double #-}

-- -- | Standard error of the mean. This is the standard deviation
-- -- divided by the square root of the sample size.
-- stdErrMean :: (G.Vector v Double) => v Double -> Double
-- stdErrMean samp = stdDev samp / (sqrt . fromIntegral . G.length) samp
-- {-# SPECIALIZE stdErrMean :: U.Vector Double -> Double #-}
-- {-# SPECIALIZE stdErrMean :: V.Vector Double -> Double #-}

-- robustSumVarWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> V
-- robustSumVarWeighted samp = G.foldl' go (V 0 0) samp
--     where
--       go (V s w) (x,xw) = V (s + xw*d*d) (w + xw)
--           where d = x - m
--       m = meanWeighted samp
-- {-# INLINE robustSumVarWeighted #-}

-- -- | Weighted variance. This is biased estimation.
-- varianceWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> Double
-- varianceWeighted samp
--     | G.length samp > 1 = fini $ robustSumVarWeighted samp
--     | otherwise         = 0
--     where
--       fini (V s w) = s / w
-- {-# SPECIALIZE varianceWeighted :: U.Vector (Double,Double) -> Double #-}
-- {-# SPECIALIZE varianceWeighted :: V.Vector (Double,Double) -> Double #-}


-- | Covariance of sample of pairs. For empty sample it's set to
--   zero
covariance :: (G.Vector v (Double,Double), G.Vector v Double, MonadThrow m)
           => v (Double,Double)
           -> m Double
covariance xy
  | n <= 0    = modErr "covariance" "Insufficient sample size"
  | otherwise = do
      muX <- mean xs
      muY <- mean ys
      mean $ G.zipWith (*)
        (G.map (\x -> x - muX) xs)
        (G.map (\y -> y - muY) ys)
   where
     n       = G.length xy
     (xs,ys) = G.unzip xy
{-# SPECIALIZE covariance :: MonadThrow m => U.Vector (Double,Double) -> m Double #-}
{-# SPECIALIZE covariance :: MonadThrow m => V.Vector (Double,Double) -> m Double #-}

-- | Correlation coefficient for sample of pairs. Also known as
--   Pearson's correlation. For empty sample it's set to zero.
correlation :: (G.Vector v (Double,Double), G.Vector v Double, MonadThrow m)
           => v (Double,Double)
           -> m Double
correlation xy
  | n <= 1    = modErr "correlation" "Insufficient sample size"
  | otherwise = do
      cov <- mean $ G.zipWith (*)
        (G.map (\x -> x - muX) xs)
        (G.map (\y -> y - muY) ys)
      return $! cov / sqrt (varX * varY)
  where
    n       = G.length xy
    (xs,ys) = G.unzip  xy
    (muX,varX) = undefined -- meanVariance xs
    (muY,varY) = undefined -- meanVariance ys
{-# SPECIALIZE correlation :: MonadThrow m => U.Vector (Double,Double) -> m Double #-}
{-# SPECIALIZE correlation :: MonadThrow m => V.Vector (Double,Double) -> m Double #-}
  
-- | Pair two samples. It's like 'G.zip' but requires that both
--   samples have equal size.
pair :: (G.Vector v a, G.Vector v b, G.Vector v (a,b)) => v a -> v b -> v (a,b)
pair va vb
  | G.length va == G.length vb = G.zip va vb
  | otherwise = error "Statistics.Sample.pair: vector must have same length"
{-# INLINE pair #-}

------------------------------------------------------------------------
-- Helper code. Monomorphic unpacked accumulators.

-- (^) operator from Prelude is just slow.
(^) :: Double -> Int -> Double
x ^ 1 = x
x ^ n = x * (x ^ (n-1))
{-# INLINE (^) #-}

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

modErr :: MonadThrow m => String -> String -> m a
modErr f err = throwM $ InvalidSample ("Statistics.Sample." ++ f) err

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
