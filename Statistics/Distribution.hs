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
    , findRoot
    ) where

-- | The interface shared by all probability distributions.
class Distribution d where
    -- | Probability density function. The probability that a
    -- stochastic variable @x@ has the value @X@, i.e. @P(x=X)@.
    probability :: d -> Double -> Double

    -- | Cumulative distribution function.  The probability that a
    -- stochastic variable @x@ is less than @X@, i.e. @P(x<X)@.
    cumulative  :: d -> Double -> Double

    -- | Inverse of the cumulative distribution function.  The value
    -- @X@ for which @P(x<X)@.
    inverse     :: d -> Double -> Double

-- | Approximate the value of @X@ for which @P(x>X) == p@.
--
-- This method uses a combination of Newton-Raphson iteration and
-- bisection with the given guess as a starting point.  The upper and
-- lower bounds specify the interval in which the probability
-- distribution reaches the value @p@.
findRoot :: Distribution d => d
         -> Double              -- ^ Probability @p@
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
        (lo',hi') | err < 0   = (x, hi)
                  | otherwise = (lo, x)
        pdf                   = probability d x
        (dx',x') | pdf /= 0   = (err / pdf, x - dx)
                 | otherwise  = (dx, x)
        (dx'',x'')
            | x' < lo' || x' > hi' || pdf == 0 = (x'-x, (lo + hi) / 2)
            | otherwise                        = (dx',  x')
    accuracy = 1e-15
    maxIters = 150
