{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Binomial
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The binomial distribution.  This is the discrete probability
-- distribution of the number of successes in a sequence of /n/
-- independent yes\/no experiments, each of which yields success with
-- probability /p/.

module Statistics.Distribution.Binomial
    (
      BinomialDistribution
    -- * Constructors
    , binomial
    -- * Accessors
    , bdTrials
    , bdProbability
    ) where

import Control.Exception (assert)
import qualified Data.Vector.Unboxed as U
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Statistics.Constants (m_epsilon)
import qualified Statistics.Distribution as D
import Statistics.Distribution.Normal (standard)
import Statistics.Math (choose, logFactorial)

-- | The binomial distribution.
data BinomialDistribution = BD {
      bdTrials      :: {-# UNPACK #-} !Int
    -- ^ Number of trials.
    , bdProbability :: {-# UNPACK #-} !Double
    -- ^ Probability.
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution BinomialDistribution where
    cumulative = cumulative

instance D.DiscreteDistr BinomialDistribution where
    probability = probability

instance D.Variance BinomialDistribution where
    variance = variance

instance D.Mean BinomialDistribution where
    mean = mean


-- This could be slow for bin n
probability :: BinomialDistribution -> Int -> Double
probability (BD n p) k 
  | k < 0 || k > n = 0
  | n == 0         = 1
  | otherwise      = choose n k * p^k * (1-p)^(n-k)
{-# INLINE probability #-}

-- Summation from different sides required to reduce roundoff errors
cumulative :: BinomialDistribution -> Double -> Double
cumulative d@(BD n p) x
  | k <  0    = 0
  | k >= n    = 1
  | k <  m    = D.sumProbabilities d 0 k
  | otherwise = 1 - D.sumProbabilities d (k+1) n
    where
      m = floor (mean d)
      k = floor x
{-# INLINE cumulative #-}

mean :: BinomialDistribution -> Double
mean (BD n p) = fromIntegral n * p
{-# INLINE mean #-}

variance :: BinomialDistribution -> Double
variance (BD n p) = fromIntegral n * p * (1 - p)
{-# INLINE variance #-}

-- | Construct binomial distribution
binomial :: Int                 -- ^ Number of trials.
         -> Double              -- ^ Probability.
         -> BinomialDistribution
binomial n p =
    assert (n > 0) .
    assert (p > 0 && p < 1) $
    BD n p
{-# INLINE binomial #-}
