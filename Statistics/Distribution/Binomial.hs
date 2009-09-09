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
import Data.Array.Vector
import qualified Statistics.Distribution as D
import Statistics.Function (choose)

-- | The binomial distribution.
data BinomialDistribution = BD {
      bdTrials      :: {-# UNPACK #-} !Int
    -- ^ Number of trials.
    , bdProbability :: {-# UNPACK #-} !Double
    -- ^ Probability.
    } deriving (Eq, Read, Show)

instance D.Distribution BinomialDistribution where
    probability = probability
    cumulative = cumulative
    inverse = inverse

instance D.Variance BinomialDistribution where
    variance = variance

instance D.Mean BinomialDistribution where
    mean = mean

probability :: BinomialDistribution -> Double -> Double
probability (BD n p) x =
    fromIntegral (n `choose` floor x) * p ** x * (1-p) ** (fromIntegral n-x)

cumulative :: BinomialDistribution -> Double -> Double
cumulative d =
    sumU . mapU (probability d . fromIntegral) . enumFromToU (0::Int) . floor

inverse :: BinomialDistribution -> Double -> Double
inverse d@(BD n _p) p = D.findRoot d p (n'/2) 0 n'
    where n' = fromIntegral n

mean :: BinomialDistribution -> Double
mean (BD n p) = fromIntegral n * p
{-# INLINE mean #-}

variance :: BinomialDistribution -> Double
variance (BD n p) = fromIntegral n * p * (1 - p)
{-# INLINE variance #-}

binomial :: Int                 -- ^ Number of trials.
         -> Double              -- ^ Probability.
         -> BinomialDistribution
binomial n p =
    assert (n > 0) .
    assert (p > 0 && p < 1) $
    BD n p
{-# INLINE binomial #-}
