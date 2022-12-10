{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
-- |
-- Module    : Statistics.Distribution
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Type classes for probability distributions

module Statistics.Distribution
    (
      -- * Type classes
      Distribution(..)
    , DiscreteDistr(..)
    , ContDistr(..)
      -- ** Distribution statistics
    , MaybeMean(..)
    , Mean(..)
    , MaybeVariance(..)
    , Variance(..)
    , MaybeEntropy(..)
    , Entropy(..)
    , FromSample(..)
      -- ** Random number generation
    , ContGen(..)
    , DiscreteGen(..)
    , genContinuous
      -- * Helper functions
    , findRoot
    , sumProbabilities
    ) where

import Prelude hiding (sum)
import Statistics.Function        (square)
import Statistics.Sample.Internal (sum)
import System.Random.Stateful     (StatefulGen, uniformDouble01M)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G


-- | Type class common to all distributions. Only c.d.f. could be
-- defined for both discrete and continuous distributions.
class Distribution d where
    -- | Cumulative distribution function.  The probability that a
    -- random variable /X/ is less or equal than /x/,
    -- i.e. P(/X/≤/x/). Cumulative should be defined for
    -- infinities as well:
    --
    -- > cumulative d +∞ = 1
    -- > cumulative d -∞ = 0
    cumulative :: d -> Double -> Double
    cumulative d x = 1 - complCumulative d x
    -- | One's complement of cumulative distribution:
    --
    -- > complCumulative d x = 1 - cumulative d x
    --
    -- It's useful when one is interested in P(/X/>/x/) and
    -- expression on the right side begin to lose precision. This
    -- function have default implementation but implementors are
    -- encouraged to provide more precise implementation.
    complCumulative :: d -> Double -> Double
    complCumulative d x = 1 - cumulative d x
    {-# MINIMAL (cumulative | complCumulative) #-}


-- | Discrete probability distribution.
class Distribution  d => DiscreteDistr d where
    -- | Probability of n-th outcome.
    probability :: d -> Int -> Double
    probability d = exp . logProbability d
    -- | Logarithm of probability of n-th outcome
    logProbability :: d -> Int -> Double
    logProbability d = log . probability d
    {-# MINIMAL (probability | logProbability) #-}

-- | Continuous probability distribution.
--
--   Minimal complete definition is 'quantile' and either 'density' or
--   'logDensity'.
class Distribution d => ContDistr d where
    -- | Probability density function. Probability that random
    -- variable /X/ lies in the infinitesimal interval
    -- [/x/,/x+/δ/x/) equal to /density(x)/⋅δ/x/
    density :: d -> Double -> Double
    density d = exp . logDensity d
    -- | Natural logarithm of density.
    logDensity :: d -> Double -> Double
    logDensity d = log . density d
    -- | Inverse of the cumulative distribution function. The value
    -- /x/ for which P(/X/≤/x/) = /p/. If probability is outside
    -- of [0,1] range function should call 'error'
    quantile :: d -> Double -> Double
    quantile d x = complQuantile d (1 - x)
    -- | 1-complement of @quantile@:
    --
    -- > complQuantile x ≡ quantile (1 - x)
    complQuantile :: d -> Double -> Double
    complQuantile d x = quantile d (1 - x)
    {-# MINIMAL (density | logDensity), (quantile | complQuantile) #-}

-- | Type class for distributions with mean. 'maybeMean' should return
--   'Nothing' if it's undefined for current value of data
class Distribution d => MaybeMean d where
    maybeMean :: d -> Maybe Double

-- | Type class for distributions with mean. If a distribution has
--   finite mean for all valid values of parameters it should be
--   instance of this type class.
class MaybeMean d => Mean d where
    mean :: d -> Double



-- | Type class for distributions with variance. If variance is
--   undefined for some parameter values both 'maybeVariance' and
--   'maybeStdDev' should return Nothing.
--
--   Minimal complete definition is 'maybeVariance' or 'maybeStdDev'
class MaybeMean d => MaybeVariance d where
    maybeVariance :: d -> Maybe Double
    maybeVariance = fmap square . maybeStdDev
    maybeStdDev   :: d -> Maybe Double
    maybeStdDev   = fmap sqrt . maybeVariance
    {-# MINIMAL (maybeVariance | maybeStdDev) #-}

-- | Type class for distributions with variance. If distribution have
--   finite variance for all valid parameter values it should be
--   instance of this type class.
--
--   Minimal complete definition is 'variance' or 'stdDev'
class (Mean d, MaybeVariance d) => Variance d where
    variance :: d -> Double
    variance d = square (stdDev d)
    stdDev   :: d -> Double
    stdDev = sqrt . variance
    {-# MINIMAL (variance | stdDev) #-}


-- | Type class for distributions with entropy, meaning Shannon entropy
--   in the case of a discrete distribution, or differential entropy in the
--   case of a continuous one.  'maybeEntropy' should return 'Nothing' if
--   entropy is undefined for the chosen parameter values.
class (Distribution d) => MaybeEntropy d where
  -- | Returns the entropy of a distribution, in nats, if such is defined.
  maybeEntropy :: d -> Maybe Double

-- | Type class for distributions with entropy, meaning Shannon
--   entropy in the case of a discrete distribution, or differential
--   entropy in the case of a continuous one.  If the distribution has
--   well-defined entropy for all valid parameter values then it
--   should be an instance of this type class.
class (MaybeEntropy d) => Entropy d where
  -- | Returns the entropy of a distribution, in nats.
  entropy :: d -> Double

-- | Generate discrete random variates which have given
--   distribution.
class Distribution d => ContGen d where
  genContVar :: (StatefulGen g m) => d -> g -> m Double

-- | Generate discrete random variates which have given
--   distribution. 'ContGen' is superclass because it's always possible
--   to generate real-valued variates from integer values
class (DiscreteDistr d, ContGen d) => DiscreteGen d where
  genDiscreteVar :: (StatefulGen g m) => d -> g -> m Int

-- | Estimate distribution from sample. First parameter in sample is
--   distribution type and second is element type.
class FromSample d a where
  -- | Estimate distribution from sample. Returns 'Nothing' if there is
  --   not enough data, or if no usable fit results from the method
  --   used, e.g., the estimated distribution parameters would be
  --   invalid or inaccurate.
  fromSample :: G.Vector v a => v a -> Maybe d


-- | Generate variates from continuous distribution using inverse
--   transform rule.
genContinuous :: (ContDistr d, StatefulGen g m) => d -> g -> m Double
genContinuous d gen = do
  x <- uniformDouble01M gen
  return $! quantile d x

data P = P {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Approximate the value of /X/ for which P(/x/>/X/)=/p/.
--
-- This method uses a combination of Newton-Raphson iteration and
-- bisection with the given guess as a starting point.  The upper and
-- lower bounds specify the interval in which the probability
-- distribution reaches the value /p/.
findRoot :: ContDistr d =>
            d                   -- ^ Distribution
         -> Double              -- ^ Probability /p/
         -> Double              -- ^ Initial guess
         -> Double              -- ^ Lower bound on interval
         -> Double              -- ^ Upper bound on interval
         -> Double
findRoot d prob = loop 0 1
  where
    loop !(i::Int) !dx !x !lo !hi
      | abs dx <= accuracy || i >= maxIters = x
      | otherwise                           = loop (i+1) dx'' x'' lo' hi'
      where
        err                   = cumulative d x - prob
        P lo' hi' | err < 0   = P x hi
                  | otherwise = P lo x
        pdf                   = density d x
        P dx' x' | pdf /= 0   = P (err / pdf) (x - dx)
                 | otherwise  = P dx x
        P dx'' x''
            | x' < lo' || x' > hi' || pdf == 0 = let y = (lo' + hi') / 2
                                                 in  P (y-x) y
            | otherwise                        = P dx' x'
    accuracy = 1e-15
    maxIters = 150

-- | Sum probabilities in inclusive interval.
sumProbabilities :: DiscreteDistr d => d -> Int -> Int -> Double
sumProbabilities d low hi =
  -- Return value is forced to be less than 1 to guard against roundoff errors.
  -- ATTENTION! this check should be removed for testing or it could mask bugs.
  min 1 . sum . U.map (probability d) $ U.enumFromTo low hi
