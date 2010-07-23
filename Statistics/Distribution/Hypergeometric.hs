{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Hypergeometric
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Hypergeometric distribution.  This is the discrete probability
-- distribution that measures the probability of /k/ successes in /l/
-- trials, without replacement, from a finite population.
--
-- The parameters of the distribution describe /k/ elements chosen
-- from a population of /l/, with /m/ elements of one type, and
-- /l/-/m/ of the other (all are positive integers).

module Statistics.Distribution.Hypergeometric
    (
      HypergeometricDistribution
    -- * Constructors
    , hypergeometric
    -- ** Accessors
    , hdM
    , hdL
    , hdK
    ) where

import Control.Exception (assert)
import Data.Typeable (Typeable)
import Statistics.Math (choose)
import qualified Statistics.Distribution as D

data HypergeometricDistribution = HD {
      hdM :: {-# UNPACK #-} !Int
    , hdL :: {-# UNPACK #-} !Int
    , hdK :: {-# UNPACK #-} !Int
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution HypergeometricDistribution where
    cumulative d x = D.sumProbabilities d 0 (floor x)

instance D.DiscreteDistr HypergeometricDistribution where
    probability = probability

instance D.Variance HypergeometricDistribution where
    variance = variance

instance D.Mean HypergeometricDistribution where
    mean = mean

variance :: HypergeometricDistribution -> Double
variance (HD m l k) = (k' * ml) * (1 - ml) * (l' - k') / (l' - 1)
  where m' = fromIntegral m
        l' = fromIntegral l
        k' = fromIntegral k
        ml = m' / l'
{-# INLINE variance #-}

mean :: HypergeometricDistribution -> Double
mean (HD m l k) = fromIntegral k * fromIntegral m / fromIntegral l
{-# INLINE mean #-}

hypergeometric :: Int               -- ^ /m/
               -> Int               -- ^ /l/
               -> Int               -- ^ /k/
               -> HypergeometricDistribution
hypergeometric m l k =
    assert (m >= 0 && m <= l) .
    assert (l > 0) .
    assert (k > 0 && k <= l) $
    HD m l k
{-# INLINE hypergeometric #-}

-- Naive implementation
probability :: HypergeometricDistribution -> Int -> Double
probability (HD mi li ki) n
  | n < max 0 (mi+ki-li) || n > min mi ki = 0
  | otherwise =
      choose mi n * choose (li - mi) (ki - n) / choose li ki
{-# INLINE probability #-}
