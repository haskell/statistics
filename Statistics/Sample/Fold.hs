{-# LANGUAGE BangPatterns #-}
-- |
-- Module    : Statistics.Sample.Fold
-- Copyright : (c) 2008 Don Stewart, 2009 Bryan O'Sullivan, 2025 Alex Mason
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Commonly used sample statistics, also known as descriptive
-- statistics. They are provided as 'Control.Foldl.Fold''s
-- allowing them to be applied to any 'Foldable' type containing
-- 'Double's. The functions in "Statistics.Sample" are all
-- implemented in terms of these folds.
--
-- The benefit of poviding these as 'Fold's is that they can be combined
-- compute multiple statistics with a single pass:
--
-- >>> -- Compute the mean divided by the variance
-- >>> F.fold (liftA2 (/) FF.mean FF.fastVariance) [1,2,3,4,5,6,7,8]
-- 0.8571428571428571
--
-- 'Fold's can be modified in several ways; use 'F.premap' to extract
-- values from the input before being passed into the fold:
--
-- >>> :t F.premap fst mean
-- F.Fold (Double, a) Double
--
--
-- They can be have their result mapped using `fmap`:
--
-- >>> -- single pass standard deviation
-- >>> :t sqrt <$> fastVariance
-- F.Fold Double Double
--
-- (They also have a 'Profunctor' instance, so also have 'lmap', 'rmap', 'dimap'
-- to contravariantly map the inputs and covariantly map output).
--
-- And they can be combined 'Applicative'ly to produce new single pass
-- combined statistics (or even non-statistical 'Fold's).
--
-- >>> differenceOfMeans = liftA2 (-) (F.premap fst mean) (F.premap snd mean)
-- >>> :t differenceOfMeans
-- F.Fold (Double, Double) Double
-- >>> F.fold differenceOfMeans [(10,1), (20,2), (30,3), (40,4)]
-- 22.5
--
-- 'Fold's also have 'Num', 'Floating', 'Fractional', `Semigroup` and 'Monoid'
-- instances, which allow them to be combined succinctly:
--
-- >>> myMean = sum / F.genericLength
-- >>> F.fold myMean [1..10.0]
-- 5.5
--
-- >>> myStdDev = sqrt fastVariance
-- >>> F.fold myStdDev [1..10]
-- 2.8722813232690143
-- >>> differenceOfMeans = F.premap fst mean - F.premap snd mean
--
--
-- === Performance
-- Inlining can make a very large difference in performance when defining 'Fold's,
-- which is why all definitions in this module are marked @INLINE@. If you define
-- 'Fold's which you export in your own modules and libraries, you should mark
-- them as @INLINE@ unless you have a good reason not to. Doing so allows their
-- definitions to be inlined into the definitions of folding functions, allowing
-- for significantly greater unboxing to occur.

module Statistics.Sample.Fold
    (
    -- * Descriptive functions
    minimum'
    , maximum'
    , range

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

    -- ** Single pass Length, Mean, Variance, Skewness and Kurtosis
    -- $lmvsk
    , LMVSK(..)
    , LMVSKState
    , fastLMVSK
    , fastLMVSKu
    , foldLMVSKState
    , getLMVSK
    , getLMVSKu

    -- * Strict types and helpers
    , F.fold
    , biExpectation
    , kbnSum
    , kbnSum'
    , kbn
    , summation
    , summationVia
    -- * References
    -- $references
    ) where

import Statistics.Function (square)
import Numeric.Sum (kbn, Summation(zero,add), KBNSum)

import qualified Control.Foldl as F

-- Operator ^ will be overridden
import Prelude hiding ((^), sum)
import Control.DeepSeq (NFData(..))


------------------------------------------------------------------------
-- Helper code. Monomorphic unpacked accumulators.

-- (^) operator from Prelude is just slow.
(^) :: Double -> Int -> Double
x ^ 1 = x
x ^ 2 = x * x
x ^ 3 = x * x * x
x ^ 4 = (x * x) * (x * x)
x ^ 5 = (x * x) * x * (x * x)
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


-- | Range. The difference between the largest and smallest
-- elements of a sample.
range :: F.Fold Double Double
range = maximum' - minimum'
{-# INLINE range #-}

-- | Maximum specialised to Double, returns -∞ without input.
maximum' :: F.Fold Double Double
maximum' = F.Fold max (-1/0 :: Double) id where
{-# INLINE minimum' #-}

-- | Minimum specialised to Double, returns ∞ without input.
minimum' :: F.Fold Double Double
minimum' = F.Fold min ( 1/0 :: Double) id where
{-# INLINE maximum' #-}

-- | Sum Doubles using Kahan-Babuška-Neumaier summation for increased accuracy. If you need to compute partial sums for combination later, use @kbnSum'@.
kbnSum :: F.Fold Double Double
kbnSum = summationVia kbn
{-# INLINE kbnSum #-}

-- | Sum Doubles using Kahan-Babuška-Neumaier summation
--   for increased accuracy. The @KBNSum@ type contains
--   the internal state of the sum, and as it is a monoid,
--   can be use to combine different sums accurately.
--
-- === __Examples:__
-- >>> xs = [1,2,pi,1e-9]
-- >>> ys = [4,5,2*pi,1e-11]
-- >>> r1 = F.fold kbnSum' xs <> F.fold kbnSum' ys
-- >>> r1
-- KBNSum 21.424777961779377 2.580967484452971e-15
--
-- >>> kbn r1
-- 21.42477796177938
-- >>> r2 = F.fold kbnSum' (xs ++ ys)
-- >>> r2
-- KBNSum 21.42477796177938 (-9.7174619434753e-16)
--
-- >>> kbn r2
-- 21.42477796177938
kbnSum' :: F.Fold Double KBNSum
kbnSum' = summation
{-# INLINE kbnSum' #-}

-- | Sum doubles using any instance of the 'Summation' class.
summation :: Summation a => F.Fold Double a
summation = F.Fold add zero id
{-# INLINE summation #-}

-- | Sum doubles by using the finalisation function for types in the
--  'Summation' class. 'kbnSum' is equivalent to @summationVia kbn@.
--
-- >>> F.fold (summationVia kbn) [1,2,pi,1e-9,1e-17]
-- 6.141592654589793
-- >>> F.fold (summationVia Numeric.Sum.kb2) [1,2,pi,1e-9,9e-15]
-- 6.141592654589802
summationVia :: Summation s => (s -> Double) -> F.Fold Double Double
summationVia f = f <$> summation
{-# INLINE summationVia #-}

-- Counts the number of elementes using Word for the count.
doubleLength :: F.Fold a Double
doubleLength = F.Fold (\n _ -> n+1) (0::Word) fromIntegral

-- | Compute expectation of function over for sample.
expectation :: (a -> Double) -> F.Fold a Double
expectation f = F.premap f kbnSum / doubleLength
{-# INLINE expectation #-}

-- | Arithmetic mean.  This uses Kahan-Babuška-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
mean :: F.Fold Double Double
mean = kbnSum / doubleLength
{-# INLINE mean #-}


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

-- | Harmonic mean.
harmonicMean :: F.Fold Double Double
harmonicMean = F.Fold go (T 0 0) fini
  where
    fini (T b a) = fromIntegral a / b
    go (T x y) n = T (x + (1/n)) (y+1)
{-# INLINE harmonicMean #-}

-- | Geometric mean of a sample containing no negative values.
geometricMean :: F.Fold Double Double
geometricMean = exp (expectation log)
{-# INLINE geometricMean #-}


-- | Compute the /k/th central moment of a sample.  The central moment
-- is also known as the moment about the mean.
--
-- The mean must be provided which may require two passes over
-- the input data.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMoment :: Int    -- ^ /k/
              -> Double -- ^ /mean/
              -> F.Fold Double Double
centralMoment a m
    | a < 0  = error "Statistics.Sample.centralMoment: negative input"
    | a == 0 = pure 1
    | a == 1 = pure 0
    | otherwise = expectation go
  where
    go x = (x-m) ^ a
{-# INLINE centralMoment #-}


-- | Compute the /k/th and /j/th central moments of a sample.
--
-- The mean must already be known.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMoments :: Int    -- ^ /i/
               -> Int    -- ^ /j/
               -> Double -- ^ /mean/
               -> F.Fold Double (Double, Double) -- ^ /i/th & /j/th central moments
centralMoments a b m
    | a < 2 || b < 2 = liftA2 (,) (centralMoment a m) (centralMoment b m)
    | otherwise      = F.Fold go (T1 0 0 0) fini
  where go (T1 n i j) x = T1 (n+1) (i + d^a) (j + d^b)
            where d  = x - m
        fini (T1 n i j) = (i / n' , j / n')
            where n' = fromIntegral n
{-# INLINE centralMoments #-}


-- | Compute the skewness of a sample. This is a measure of the
-- asymmetry of its distribution. The mean must already be known.
--
-- A sample with negative skew is said to be /left-skewed/.  Most of
-- its mass is on the right of the distribution, with the tail on the
-- left.
--
-- >>> xs = [1,100,101,102,103]
-- >>> m  = F.fold mean xs
-- >>> F.fold (skewness m) xs
-- -1.4976814499182607
--
-- > ==> -1.497681449918257
--
-- A sample with positive skew is said to be /right-skewed/.
--
-- >>> xs = [1,2,3,4,100]
-- >>> m  = F.fold mean xs
-- >>> F.fold (skewness m) xs
-- 1.4975367033335198
--
-- A sample's skewness is not defined if its 'variance' is zero.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
skewness :: Double -> F.Fold Double Double
skewness m = (\(c3, c2) -> c3 * c2 ** (-1.5)) <$> centralMoments 3 2 m
{-# INLINE skewness #-}


-- | Compute the excess kurtosis of a sample.  This is a measure of
-- the \"peakedness\" of its distribution.  A high kurtosis indicates
-- that more of the sample's variance is due to infrequent severe
-- deviations, rather than more frequent modest deviations. The mean
-- must already be known.
--
-- A sample's excess kurtosis is not defined if its 'variance' is
-- zero.
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
-- The mean must already be known, and may require two passes over
-- the data.

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance, where the denominator is /n/.
variance :: Double -> F.Fold Double Double
variance m =
    liftA2 (\s n -> if n > 1 then s / n else 0)
           (robustSumVar m) doubleLength
{-# INLINE variance #-}


-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance, where the denominator is /n/-1.
varianceUnbiased :: Double -> F.Fold Double Double
varianceUnbiased m =
    liftA2 (\s n -> if n > 1 then s / (n-1) else 0)
           (robustSumVar m) doubleLength
{-# INLINE varianceUnbiased #-}


-- | The square of differences from the mean:
--
-- \(robustSumVar(xs) = \sum_{i=0}^n{(x_i-m)^2}\)
--
--'variance' is the result of this divided by the number of inputs.
robustSumVar :: Double -> F.Fold Double Double
robustSumVar m = F.premap (square . subtract m) kbnSum
{-# INLINE robustSumVar #-}

-- | Standard deviation.  This is simply the square root of the
-- unbiased estimate of the variance.
stdDev :: Double -> F.Fold Double Double
stdDev m = sqrt (varianceUnbiased m)
{-# INLINE stdDev #-}

-- | Standard error of the mean. This is the standard deviation
-- divided by the square root of the sample size.
stdErrMean :: Double -> F.Fold Double Double
stdErrMean m = stdDev m / sqrt doubleLength
{-# INLINE stdErrMean #-}

robustSumVarWeighted :: Double -> F.Fold (Double,Double) (Double, Double)
robustSumVarWeighted wm = F.Fold go (V 0 0) (\(V a b) -> (a,b))
    where
      go (V s w) (x,xw) = V (s + xw*d*d) (w + xw)
          where d = x - wm
{-# INLINE robustSumVarWeighted #-}

-- | Weighted variance. This is biased estimation.
varianceWeighted :: Double -> F.Fold (Double,Double) Double
varianceWeighted wm = fini <$> robustSumVarWeighted wm
    where
      fini (s, w) = s / w
{-# INLINE varianceWeighted #-}

-- $cancellation
--
-- The functions prefixed with the name @fast@ below perform a single
-- pass over the sample data using Knuth's algorithm. They usually
-- work well, but see below for caveats.
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
{-# INLINE fastVar #-}

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
fastStdDev = sqrt fastVariance
{-# INLINE fastStdDev #-}

-- | Compute two expectations at once, to avoid computing the length twice.
biExpectation :: (a -> Double) -> (a -> Double) -> F.Fold a (Double, Double)
biExpectation f s = fini <$> doubleLength <*> F.premap f kbnSum <*> F.premap s kbnSum
    where fini n a b =  (a/n, b/n)
{-# INLINE biExpectation #-}

-- | Covariance of sample of pairs. For empty sample it's set to
--   zero. The means `muX` and `muY` must already be known.
covariance ::(Double, Double) -> F.Fold (Double,Double) Double
covariance (muX, muY) =
    liftA2 (\n x -> if n > 0 then x else 0)
        F.length
        (expectation (\(x,y) -> (x - muX)*(y - muY)))
{-# INLINE covariance #-}


-- | Correlation coefficient for sample of pairs. Also known as
--   Pearson's correlation. For empty sample it's set to zero.
--   The means `muX` and `muY` must already be known.
correlation :: (Double, Double) -> F.Fold (Double,Double) Double
correlation (muX, muY) =
    fini <$> F.length <*> covF <*> varsF
  where
    fini n cov (varX, varY)
        | n == 0 = 0
        | otherwise = cov / sqrt (varX * varY)
    covF  = expectation (\(x,y) -> (x - muX)*(y - muY))
    varsF = biExpectation (\(x,_) -> square (x - muX))
                          (\(_,y) -> square (y - muY))
{-# INLINE correlation #-}

-- $lmvsk
--
-- 'LMVSK' represents the Length, Mean, Variance, Skewness
-- and Kurtosis of a sample. If you need to compute several
-- of these statistics at once, using these folds may be more
-- efficient than combining the 'Fold's above Applicatively.
--
-- The algorithm used is similar to the found in 'fastVariance' etc.
-- but common subexpressions are shared.

data LMVSK  = LMVSK
  { lmvskLength   :: {-# UNPACK #-}!Int
  , lmvskMean     :: {-# UNPACK #-}!Double
  , lmvskVariance :: {-# UNPACK #-}!Double
  , lmvskSkewness :: {-# UNPACK #-}!Double
  , lmvskKurtosis :: {-# UNPACK #-}!Double
  } deriving (Show, Eq)


-- | Intermediate state for computing 'LMVSK'. This type's 'Semigroup'
-- instance allows it to be computed in parallel and combined
newtype LMVSKState = LMVSKState LMVSK

instance NFData LMVSK      where rnf !_ = ()
instance NFData LMVSKState where rnf !_ = ()

instance Monoid LMVSKState where
  {-# INLINE mempty #-}
  mempty = LMVSKState lmvskZero
  {-# INLINE mappend #-}
  mappend = (<>)

instance Semigroup LMVSKState where
  {-# INLINE (<>) #-}
  (LMVSKState (LMVSK an am1 am2 am3 am4)) <> (LMVSKState (LMVSK bn bm1 bm2 bm3 bm4))
    = LMVSKState (LMVSK n m1 m2 m3 m4) where
    fi :: Int -> Double
    fi = fromIntegral
    -- combined.n = a.n + b.n;
    n      = an+bn
    n2     = n*n
    nd     = fi n
    nda    = fi an
    ndb    = fi bn
    -- delta = b.M1 - a.M1;
    delta  =    bm1 - am1
    -- delta2 = delta*delta;
    delta2 =    delta*delta
    -- delta3 = delta*delta2;
    delta3 =    delta*delta2
    -- delta4 = delta2*delta2;
    delta4 =    delta2*delta2
    -- combined.M1 = (a.n*a.M1 + b.n*b.M1) / combined.n;
    m1     =         (nda*am1  + ndb*bm1 ) / nd
    -- combined.M2 = a.M2 + b.M2 + delta2*a.n*b.n / combined.n;
    m2     =          am2 + bm2  + delta2*nda*ndb / nd
    -- combined.M3 = a.M3 + b.M3 + delta3*a.n*b.n*   (a.n - b.n)/(combined.n*combined.n);
    m3     =         am3  + bm3  + delta3*nda*ndb* fi( an - bn )/ fi n2
    -- combined.M3 += 3.0*delta * (a.n*b.M2 - b.n*a.M2) / combined.n;
           +          3.0*delta * (nda*bm2  - ndb*am2 ) / nd
    --
    -- combined.M4 = a.M4 + b.M4 + delta4*a.n*b.n * (a.n*a.n - a.n*b.n + b.n*b.n) /(combined.n*combined.n*combined.n);
    m4     =         am4  + bm4  + delta4*nda*ndb *fi(an*an  -  an*bn  +  bn*bn ) / fi (n*n*n)
    -- combined.M4 += 6.0*delta2 * (a.n*a.n*b.M2 + b.n*b.n*a.M2)/(combined.n*combined.n) +
           +          6.0*delta2 * (nda*nda*bm2  + ndb*ndb*am2) / fi n2
    --               4.0*delta*(a.n*b.M3 - b.n*a.M3) / combined.n;
           +         4.0*delta*(nda*bm3  - ndb*am3) / nd

-- | Efficiently compute the
-- __length, mean, variance, skewness and kurtosis__ with a single pass.
{-# INLINE fastLMVSK #-}
fastLMVSK :: F.Fold Double LMVSK
fastLMVSK = getLMVSK <$> foldLMVSKState

-- | Efficiently compute the
-- __length, mean, unbiased variance, skewness and kurtosis__ with a single pass.
{-# INLINE fastLMVSKu #-}
fastLMVSKu :: F.Fold Double LMVSK
fastLMVSKu = getLMVSKu <$> foldLMVSKState

-- | Initial 'LMVSKState'
{-# INLINE lmvskZero #-}
lmvskZero :: LMVSK
lmvskZero = LMVSK 0 0 0 0 0

-- | Performs the heavy lifting of fastLMVSK. This is exposed
--   because the internal `LMVSKState` is monoidal, allowing you
--   to run these statistics in parallel over datasets which are
--   split and then combine the results.
{-# INLINE foldLMVSKState #-}
foldLMVSKState :: F.Fold Double LMVSKState
foldLMVSKState = F.Fold stepLMVSKState (LMVSKState lmvskZero) id

{-# INLINE stepLMVSKState #-}
stepLMVSKState :: LMVSKState -> Double -> LMVSKState
stepLMVSKState (LMVSKState (LMVSK n1 m1 m2 m3 m4)) x = LMVSKState $ LMVSK n m1' m2' m3' m4' where
  fi :: Int -> Double
  fi = fromIntegral
  -- long long n1 = n;
  -- n++;
  n = n1+1
  -- delta = x - M1;
  delta =    x - m1
  -- delta_n = delta / n;
  delta_n =    delta / fi n
  -- delta_n2 = delta_n * delta_n;
  delta_n2 =    delta_n * delta_n
  -- term1 = delta * delta_n * n1;
  term1 =    delta * delta_n * fi n1
  -- M1 +=   delta_n;
  m1' = m1 + delta_n
  -- M4 +=   term1 * delta_n2 *    (n*n - 3*n + 3) + 6 * delta_n2 * M2 - 4 * delta_n * M3;
  m4' = m4 + term1 * delta_n2 * fi (n*n - 3*n + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3
  -- M3 +=   term1 * delta_n *    (n - 2) - 3 * delta_n * M2;
  m3' = m3 + term1 * delta_n * fi (n - 2) - 3 * delta_n * m2
  -- M2 +=  term1;
  m2' = m2 + term1

-- | Returns the stats which have been computed in a LMVSKState with /population/ variance.
getLMVSK :: LMVSKState -> LMVSK
getLMVSK (LMVSKState (LMVSK n m1 m2 m3 m4)) = LMVSK n m1 m2' m3' m4' where
  nd = fromIntegral n
  -- M2/(n-1.0)
  m2' = m2 / nd
  --    sqrt(double(n)) * M3/ pow(M2, 1.5)
  m3' = sqrt nd * m3 / (m2 ** 1.5)
  -- double(n)*M4 / (M2*M2) - 3.0
  m4' = nd*m4     / (m2*m2) - 3.0

-- | Returns the stats which have been computed in a LMVSKState,
--   with the /sample/ variance.
getLMVSKu :: LMVSKState -> LMVSK
getLMVSKu (LMVSKState (LMVSK n m1 m2 m3 m4)) = LMVSK n m1 m2' m3' m4' where
  nd = fromIntegral n
  -- M2/(n-1.0)
  m2' = m2 / (nd-1)
  --    sqrt(double(n)) * M3/ pow(M2, 1.5)
  m3' = sqrt nd * m3 / (m2 ** 1.5)
  -- double(n)*M4 / (M2*M2) - 3.0
  m4' = nd*m4     / (m2*m2) - 3.0


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
--
-- * John D. Cook. Computing skewness and kurtosis in one pass
--   <http://www.johndcook.com/blog/skewness_kurtosis/>
