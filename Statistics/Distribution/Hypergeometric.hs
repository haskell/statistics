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
    , fromParams
    -- ** Accessors
    , hdM
    , hdL
    , hdK
    ) where

import Control.Exception (assert)
import Data.Array.Vector
import Data.Typeable (Typeable)
import Statistics.Math (choose, logFactorial)
import Statistics.Constants (m_max_exp)
import qualified Statistics.Distribution as D

data HypergeometricDistribution = HD {
      hdM :: {-# UNPACK #-} !Int
    , hdL :: {-# UNPACK #-} !Int
    , hdK :: {-# UNPACK #-} !Int
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution HypergeometricDistribution where
    density    = density
    cumulative = cumulative
    inverse    = inverse

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

fromParams :: Int               -- ^ /m/
           -> Int               -- ^ /l/
           -> Int               -- ^ /k/
           -> HypergeometricDistribution
fromParams m l k =
    assert (m > 0 && m <= l) .
    assert (l > 0) .
    assert (k > 0 && k <= l) $
    HD m l k
{-# INLINE fromParams #-}

density :: HypergeometricDistribution -> Double -> Double
density (HD mi li ki) x
    | l <= 70    = (mi <> xi) * ((li - mi) <> (ki - xi)) / (li <> ki)
    | r > maxVal = 1/0
    | otherwise  = exp r
  where
    a <> b = fromIntegral (a `choose` b)
    r = f m + f (l-m) - f l - f xi - f (k-xi) + f k -
        f (m-xi) - f (l-m-k+xi) + f (l-k)
    f = logFactorial
    maxVal = fromIntegral (m_max_exp - 1) * log 2
    xi = floor x
    m = fromIntegral mi
    l = fromIntegral li
    k = fromIntegral ki
{-# INLINE density #-}

cumulative :: HypergeometricDistribution -> Double -> Double
cumulative d@(HD m l k) x
    | x < fromIntegral imin  = 0
    | x >= fromIntegral imax = 1
    | otherwise = min r 1
  where
    imin = max 0 (k - l + m)
    imax = min k m
    r = sumU . mapU (density d . fromIntegral) . enumFromToU imin . floor $ x
{-# INLINE cumulative #-}

inverse :: HypergeometricDistribution -> Double -> Double
inverse = error "Statistics.Distribution.Hypergeometric.inverse: not yet implemented"
{-# INLINE inverse #-}
