{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Numeric.SpecFunctions (choose)
import qualified Statistics.Distribution as D

data HypergeometricDistribution = HD {
      hdM :: {-# UNPACK #-} !Int
    , hdL :: {-# UNPACK #-} !Int
    , hdK :: {-# UNPACK #-} !Int
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary HypergeometricDistribution

instance D.Distribution HypergeometricDistribution where
    cumulative = cumulative

instance D.DiscreteDistr HypergeometricDistribution where
    probability = probability

instance D.Mean HypergeometricDistribution where
    mean = mean

instance D.Variance HypergeometricDistribution where
    variance = variance

instance D.MaybeMean HypergeometricDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance HypergeometricDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance



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
hypergeometric m l k
  | not (l > 0)            = error $ msg ++ "l must be positive"
  | not (m >= 0 && m <= l) = error $ msg ++ "m must lie in [0,l] range"
  | not (k > 0 && k <= l)  = error $ msg ++ "k must lie in (0,l] range"
  | otherwise = HD m l k
    where
      msg = "Statistics.Distribution.Hypergeometric.hypergeometric: "
{-# INLINE hypergeometric #-}

-- Naive implementation
probability :: HypergeometricDistribution -> Int -> Double
probability (HD mi li ki) n
  | n < max 0 (mi+ki-li) || n > min mi ki = 0
  | otherwise =
      choose mi n * choose (li - mi) (ki - n) / choose li ki
{-# INLINE probability #-}

cumulative :: HypergeometricDistribution -> Double -> Double
cumulative d@(HD mi li ki) x
  | isNaN x      = error "Statistics.Distribution.Hypergeometric.cumulative: NaN argument"
  | isInfinite x = if x > 0 then 1 else 0
  | n <  minN    = 0
  | n >= maxN    = 1
  | otherwise    = D.sumProbabilities d minN n
  where
    n    = floor x
    minN = max 0 (mi+ki-li)
    maxN = min mi ki
