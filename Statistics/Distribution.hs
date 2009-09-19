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
-- Types and functions common to many probability distributions.

module Statistics.Distribution
    (
      Distribution(..)
    , Mean(..)
    , Variance(..)
    , findRoot
    ) where

-- | The interface shared by all probability distributions.
class Distribution d where
    -- | Probability density function. The probability that a
    -- the random variable /X/ has the value /x/, i.e. P(/X/=/x/).
    density :: d -> Double -> Double

    -- | Cumulative distribution function.  The probability that a
    -- random variable /X/ is less than /x/, i.e. P(/X/&#8804;/x/).
    cumulative :: d -> Double -> Double

    -- | Inverse of the cumulative distribution function.  The value
    -- /x/ for which P(/X/&#8804;/x/).
    quantile :: d -> Double -> Double

class Distribution d => Mean d where
    mean :: d -> Double

class Mean d => Variance d where
    variance :: d -> Double

data P = P {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Approximate the value of /X/ for which P(/x/>/X/)=/p/.
--
-- This method uses a combination of Newton-Raphson iteration and
-- bisection with the given guess as a starting point.  The upper and
-- lower bounds specify the interval in which the probability
-- distribution reaches the value /p/.
findRoot :: Distribution d => d
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
