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
import Data.Array.Vector
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
    density    = density
    cumulative = cumulative
    quantile   = quantile

instance D.Variance BinomialDistribution where
    variance = variance

instance D.Mean BinomialDistribution where
    mean = mean

density :: BinomialDistribution -> Double -> Double
density (BD n p) x
    | isIntegral x = (n `choose` floor x) * p ** x * (1-p) ** (fromIntegral n-x)
    | otherwise    = integralError "density"

cumulative :: BinomialDistribution -> Double -> Double
cumulative d x
  | isIntegral x = sumU . mapU (density d . fromIntegral) . enumFromToU (0::Int) . floor $ x
  | otherwise    = integralError "cumulative"

isIntegral :: Double -> Bool
isIntegral x = x == floorf x

floorf :: Double -> Double
floorf = fromIntegral . (floor :: Double -> Int64)

quantile :: BinomialDistribution -> Double -> Double
quantile d@(BD n p) prob
    | isNaN prob = prob
    | p == 1     = n'
    | otherwise   = search y z 1
  where q  = 1 - p
        n' = fromIntegral n
        µ  = n' * p
        σ  = sqrt (n' * p * q)
        γ  = (q - p) / σ
        y  = n' `min` floorf (µ + σ * (d + γ * (d * d - 1) / 6) + 0.5)
          where d = D.quantile standard prob
        z  = cumulative d y
        search y z dy | z >= prob' = left y
                      | otherwise  = right y
          where
            prob' = prob * (1 - 64 * m_epsilon)
            left y | y == 0 || z < prob' = y
                   | otherwise           = left (max 0 y')
                where z  = cumulative d y'
                      y' = y - dy
            right y | y' >= n' || z >= prob' = y'
                    | otherwise              = right y'
                where z  = cumulative d y'
                      y' = y + dy

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

integralError :: String -> a
integralError f = error ("Statistics.Distribution.Binomial." ++ f ++
                         ": non-integer-valued input")
