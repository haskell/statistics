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
-- Types classes for probability distrubutions

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
      -- ** Random number generation
    , ContGen(..)
    , DiscreteGen(..)
    , genContinous
      -- * Helper functions
    , findRoot
    , sumProbabilities
    ) where

import Control.Applicative     ((<$>), Applicative(..))
import Control.Monad.Primitive (PrimMonad,PrimState)

import qualified Data.Vector.Unboxed as U
import System.Random.MWC



-- | Type class common to all distributions. Only c.d.f. could be
-- defined for both discrete and continous distributions.
class Distribution d where
    -- | Cumulative distribution function.  The probability that a
    -- random variable /X/ is less or equal than /x/,
    -- i.e. P(/X/&#8804;/x/). Cumulative should be defined for
    -- infinities as well:
    --
    -- > cumulative d +∞ = 1
    -- > cumulative d -∞ = 0
    cumulative :: d -> Double -> Double

    -- | One's complement of cumulative distibution:
    --
    -- > complCumulative d x = 1 - cumulative d x
    --
    -- It's useful when one is interested in P(/X/</x/) and
    -- expression on the right side begin to lose precision. This
    -- function have default implementation but implementors are
    -- encouraged to provide more precise implementation.
    complCumulative :: d -> Double -> Double
    complCumulative d x = 1 - cumulative d x

-- | Discrete probability distribution.
class Distribution  d => DiscreteDistr d where
    -- | Probability of n-th outcome.
    probability :: d -> Int -> Double


-- | Continuous probability distributuion
class Distribution d => ContDistr d where
    -- | Probability density function. Probability that random
    -- variable /X/ lies in the infinitesimal interval
    -- [/x/,/x+/&#948;/x/) equal to /density(x)/&#8901;&#948;/x/
    density :: d -> Double -> Double

    -- | Inverse of the cumulative distribution function. The value
    -- /x/ for which P(/X/&#8804;/x/) = /p/. If probability is outside
    -- of [0,1] range function should call 'error'
    quantile :: d -> Double -> Double



-- | Type class for distributions with mean. 'maybeMean' should return
--   'Nothing' if it's undefined for current value of data
class Distribution d => MaybeMean d where
    maybeMean :: d -> Maybe Double

-- | Type class for distributions with mean. If distribution have
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
    maybeVariance d = (*) <$> x <*> x where x = maybeStdDev d
    maybeStdDev   :: d -> Maybe Double
    maybeStdDev = fmap sqrt . maybeVariance

-- | Type class for distributions with variance. If distibution have
--   finite variance for all valid parameter values it should be
--   instance of this type class.
--
--   Minimal complete definition is 'variance' or 'stdDev'
class (Mean d, MaybeVariance d) => Variance d where
    variance :: d -> Double
    variance d = x * x where x = stdDev d
    stdDev   :: d -> Double
    stdDev = sqrt . variance


-- | Generate discrete random variates which have given
--   distribution.
class Distribution d => ContGen d where
  genContVar :: PrimMonad m => d -> Gen (PrimState m) -> m Double

-- | Generate discrete random variates which have given
--   distribution. 'ContGen' is superclass because it's always possible
--   to generate real-valued variates from integer values
class (DiscreteDistr d, ContGen d) => DiscreteGen d where
  genDiscreteVar :: PrimMonad m => d -> Gen (PrimState m) -> m Int

-- | Generate variates from continous distribution using inverse
--   transform rule.
genContinous :: (ContDistr d, PrimMonad m) => d -> Gen (PrimState m) -> m Double
genContinous d gen = do
  x <- uniform gen
  return $! quantile d x
{-# INLINE genContinous #-}

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
  -- Return value is forced to be less than 1 to guard againist roundoff errors. 
  -- ATTENTION! this check should be removed for testing or it could mask bugs.
  min 1 . U.sum . U.map (probability d) $ U.enumFromTo low hi
{-# INLINE sumProbabilities #-}