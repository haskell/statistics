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

import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import Statistics.Math (choose)

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


-- This could be slow for big n
probability :: BinomialDistribution -> Int -> Double
probability (BD n p) k 
  | k < 0 || k > n = 0
  | n == 0         = 1
  | otherwise      = choose n k * p^k * (1-p)^(n-k)
{-# INLINE probability #-}

-- Summation from different sides required to reduce roundoff errors
cumulative :: BinomialDistribution -> Double -> Double
cumulative d@(BD n _) x
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

-- | Construct binomial distribution. Number of trials must be
--   positive and probability must be in [0,1] range
binomial :: Int                 -- ^ Number of trials.
         -> Double              -- ^ Probability.
         -> BinomialDistribution
binomial n p 
  | n <= 0         = 
    error $ msg ++ "number of trials must be positive. Got " ++ show n
  | p < 0 || p > 1 = 
    error $ msg++"probability must be in [0,1] range. Got " ++ show p
  | otherwise      = BD n p
    where msg = "Statistics.Distribution.Binomial.binomial: "
{-# INLINE binomial #-}
