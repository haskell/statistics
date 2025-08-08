{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
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

module Statistics.Sample.Fold
    (
    -- * Descriptive functions
    range
    , minimum'
    , maximum'

    -- * Statistics of location
    , expectation
    , mean
    , welfordMean
    , meanWeighted
    , harmonicMean
    , geometricMean

    -- * Statistics of dispersion
    -- $variance

    -- ** Functions over central moments
    , centralMoment
    , centralMoments
    , skewness
    , kurtosis

    -- ** Two-pass functions (numerically robust)
    -- $robust
    , variance
    , varianceUnbiased
    -- , meanVariance
    -- , meanVarianceUnb
    , stdDev
    , varianceWeighted
    , stdErrMean
    , robustSumVar
    , robustSumVarWeighted

    -- ** Single-pass functions (faster, less safe)
    -- $cancellation
    , fastVariance
    , fastVarianceUnbiased
    , fastStdDev

    -- * Joint distributions
    , covariance
    , correlation
    -- * Strict types and helpers
    , V(..)
    , biExpectation
    , kbnSum
    , kbnSum'
    -- * References
    -- $references


    ) where

import Statistics.Function (square)
import Numeric.Sum (kbn, Summation(zero,add), KBNSum)

import qualified Control.Foldl as F

-- Operator ^ will be overridden
import Prelude hiding ((^), sum)


------------------------------------------------------------------------
-- Helper code. Monomorphic unpacked accumulators.

-- (^) operator from Prelude is just slow.
(^) :: Double -> Int -> Double
x ^ 1 = x
-- x ^ n = x * (x ^ (n-1))
x0 ^ n0 = go (n0-1) x0 where
    go 0 !acc = acc
    go n acc = go (n-1) (acc*x0)
{-# INLINE (^) #-}


data V  = V  {-# UNPACK #-}!Double {-# UNPACK #-}!Double
data T  = T  {-# UNPACK #-}!Double {-# UNPACK #-}!Int
data T1 = T1 {-# UNPACK #-}!Int    {-# UNPACK #-}!Double {-# UNPACK #-}!Double

-- don't support polymorphism, as we can't get unboxed returns if we use it.
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


-- | /O(n)/ Range. The difference between the largest and smallest
-- elements of a sample.
range :: F.Fold Double Double
range = maximum' - minimum'
{-# INLINE range #-}

maximum' :: F.Fold Double Double
maximum' = F.Fold max (-1/0 :: Double) id where
{-# INLINE minimum' #-}

minimum' :: F.Fold Double Double
minimum' = F.Fold min ( 1/0 :: Double) id where
{-# INLINE maximum' #-}

kbnSum' :: F.Fold Double KBNSum
kbnSum' = F.Fold add zero id
{-# INLINE kbnSum' #-}

kbnSum :: F.Fold Double Double
kbnSum = fmap kbn kbnSum'
{-# INLINE kbnSum #-}


-- | Compute expectation of function over for sample.
expectation :: (a -> Double) -> F.Fold a Double
expectation f = F.premap f kbnSum / doubleLength
{-# INLINE expectation #-}

-- | Arithmetic mean.  This uses Kahan-Babuška-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large. This function is not subject to stream
-- fusion.
mean :: F.Fold Double Double
mean = kbnSum / doubleLength
{-# INLINE mean #-}

doubleLength :: F.Fold a Double
doubleLength = F.Fold (\n _ -> n+1) (0::Int) fromIntegral


-- | Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
--
-- Compared to 'mean', this loses a surprising amount of precision
-- unless the inputs are very large.
welfordMean :: F.Fold Double Double
welfordMean = F.Fold go (T 0 0) fini
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
        where m' = m + (x - m) / fromIntegral n'
              n' = n + 1


-- | Arithmetic mean for weighted sample. It uses a single-pass
-- algorithm analogous to the one used by 'welfordMean'.
meanWeighted :: F.Fold (Double,Double) Double
meanWeighted = F.Fold go (V 0 0) fini
    where
      fini (V a _) = a
      go (V m w) (x,xw) = V m' w'
          where m' | w' == 0   = 0
                   | otherwise = m + xw * (x - m) / w'
                w' = w + xw
{-# INLINE meanWeighted #-}

-- | Harmonic mean.  This algorithm performs a single pass over
-- the sample.
harmonicMean :: F.Fold Double Double
harmonicMean = F.Fold go (T 0 0) fini
  where
    fini (T b a) = fromIntegral a / b
    go (T x y) n = T (x + (1/n)) (y+1)
{-# INLINE harmonicMean #-}

-- | Geometric mean of a sample containing no negative values.
geometricMean :: F.Fold Double Double
geometricMean = fmap exp (expectation log)
{-# INLINE geometricMean #-}


-- | Compute the /k/th central moment of a sample.  The central moment
-- is also known as the moment about the mean.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMoment :: Int
              -> Double -- ^ mean
              -> F.Fold Double Double
centralMoment a m
    | a < 0  = error "Statistics.Sample.centralMoment: negative input"
    | a == 0 = pure 1
    | a == 1 = pure 0
    | otherwise = expectation go
  where
    go x = (x-m) ^ a
    -- m    = mean xs
{-# INLINE centralMoment #-}


-- | Compute the /k/th and /j/th central moments of a sample.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMoments :: Int -> Int -> Double  -> F.Fold Double (Double, Double)
centralMoments a b m
    | a < 2 || b < 2 = liftA2 (,) (centralMoment a m) (centralMoment b m)
    | otherwise      = F.Fold go (T1 0 0 0) fini
  where go (T1 n i j) x = T1 (n+1) (i + d^a) (j + d^b)
            where d  = x - m
        fini (T1 n i j) = (i / n' , j / n')
            where n' = fromIntegral n
{-# INLINE centralMoments #-}


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
skewness :: Double -> F.Fold Double Double
skewness m = (\(c3, c2) -> c3 * c2 ** (-1.5)) <$> centralMoments 3 2 m
{-# INLINE skewness #-}


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
kurtosis :: Double -> F.Fold Double Double
kurtosis m = (\(c4, c2) -> c4 / (c2 * c2) - 3) <$> centralMoments 4 2 m
{-# INLINE kurtosis #-}


-- $variance
--
-- The variance — and hence the standard deviation — of a
-- sample of fewer than two elements are both defined to be zero.

-- $robust
--
-- These functions use the compensated summation algorithm of Chan et
-- al. for numerical robustness, but require two passes over the
-- sample data as a result.
--
-- Because of the need for two passes, these functions are /not/
-- subject to stream fusion.

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance, where the denominator is /n/.
variance :: Double -> F.Fold Double Double
variance m =
    liftA2 (\s n -> if n > 1 then s / n else 0)
           (robustSumVar m) doubleLength
{-# INLINE variance #-}


robustSumVar :: Double -> F.Fold Double Double
robustSumVar m = F.premap (square . subtract m) kbnSum
{-# INLINE robustSumVar #-}


-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance, where the denominator is /n/-1.
varianceUnbiased :: Double -> F.Fold Double Double
varianceUnbiased m =
    liftA2 (\s n -> if n > 1 then s / (n-1) else 0)
           (robustSumVar m) doubleLength
{-# INLINE varianceUnbiased #-}


{-
-- | Calculate mean and maximum likelihood estimate of variance. This
-- function should be used if both mean and variance are required
-- since it will calculate mean only once.
-- meanVariance :: Double -> F.Fold Double (Double,Double)
-- meanVariance m
--   | n > 1     = (m, robustSumVar m samp / fromIntegral n)
--   | otherwise = (m, 0)
--     where
--       n = G.length samp
-- {-# SPECIALIZE meanVariance :: U.Vector Double -> (Double,Double) #-}
-- {-# SPECIALIZE meanVariance :: V.Vector Double -> (Double,Double) #-}

-- | Calculate mean and unbiased estimate of variance. This
-- function should be used if both mean and variance are required
-- since it will calculate mean only once.
-- meanVarianceUnb :: (G.Vector v Double) => v Double -> (Double,Double)
-- meanVarianceUnb samp
--   | n > 1     = (m, robustSumVar m samp / fromIntegral (n-1))
--   | otherwise = (m, 0)
--     where
--       n = G.length samp
--       m = mean samp
-- {-# SPECIALIZE meanVarianceUnb :: U.Vector Double -> (Double,Double) #-}
-- {-# SPECIALIZE meanVarianceUnb :: V.Vector Double -> (Double,Double) #-}
-}

-- | Standard deviation.  This is simply the square root of the
-- unbiased estimate of the variance.
stdDev :: Double -> F.Fold Double Double
stdDev m = fmap sqrt $ varianceUnbiased m
{-# INLINE stdDev #-}

-- | Standard error of the mean. This is the standard deviation
-- divided by the square root of the sample size.
stdErrMean :: Double -> F.Fold Double Double
stdErrMean m = stdDev m / fmap sqrt doubleLength
{-# INLINE stdErrMean #-}

robustSumVarWeighted :: Double -> F.Fold (Double,Double) V
robustSumVarWeighted wm = F.Fold go (V 0 0) id
    where
      go (V s w) (x,xw) = V (s + xw*d*d) (w + xw)
          where d = x - wm
{-# INLINE robustSumVarWeighted #-}

-- | Weighted variance. This is biased estimation.
varianceWeighted :: Double -> F.Fold (Double,Double) Double
varianceWeighted wm = fini <$> robustSumVarWeighted wm
    where
      fini (V s w) = s / w

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

fastVar ::  F.Fold Double T1
fastVar = F.Fold go (T1 0 0 0) id
  where
    go (T1 n m s) x = T1 n' m' s'
      where n' = n + 1
            m' = m + d / fromIntegral n'
            s' = s + d * (x - m')
            d  = x - m

-- | Maximum likelihood estimate of a sample's variance.
fastVariance :: F.Fold Double Double
fastVariance = fmap fini fastVar
  where fini (T1 n _m s)
          | n > 1     = s / fromIntegral n
          | otherwise = 0
{-# INLINE fastVariance #-}

-- | Unbiased estimate of a sample's variance.
fastVarianceUnbiased :: F.Fold Double Double
fastVarianceUnbiased = fmap fini fastVar
  where fini (T1 n _m s)
          | n > 1     = s / fromIntegral (n - 1)
          | otherwise = 0
{-# INLINE fastVarianceUnbiased #-}

-- | Standard deviation.  This is simply the square root of the
-- maximum likelihood estimate of the variance.
fastStdDev :: F.Fold Double Double
fastStdDev = fmap sqrt fastVariance
{-# INLINE fastStdDev #-}

-- Avoids computing the length twice
biExpectation :: (a -> Double) -> (a -> Double) -> F.Fold a V
biExpectation f s = fini <$> F.length <*> F.premap f kbnSum <*> F.premap s kbnSum
    where fini n a b = let n' = fromIntegral n in V (a/n') (b/n')
{-# INLINE biExpectation #-}

-- | Covariance of sample of pairs. For empty sample it's set to
--   zero
covariance ::(Double, Double) -> F.Fold (Double,Double) Double
covariance (muX, muY) =
    liftA2 (\n x -> if n > 0 then x else 0)
        F.length
        (expectation (\(x,y) -> (x - muX)*(y - muY)))
{-# INLINE covariance #-}


-- | Correlation coefficient for sample of pairs. Also known as
--   Pearson's correlation. For empty sample it's set to zero.
--  The means `muX` and `muY` must be known.
correlation :: (Double, Double) -> F.Fold (Double,Double) Double
correlation (muX, muY) =
    fini <$> F.length <*> covF <*> varsF
  where
    fini n cov (V varX varY)
        | n == 0 = 0
        | otherwise = cov / sqrt (varX * varY)
    covF  = expectation (\(x,y) -> (x - muX)*(y - muY))
    varsF = biExpectation (\(x,_) -> square (x - muX))
                          (\(_,y) -> square (y - muY))
{-# INLINE correlation #-}

--------

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
