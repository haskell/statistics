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
    | n' < 1e5   = fst (search 1 y z)
    | otherwise  = let dy = floorf (n' / 1000)
                   in  narrow dy (search dy y z)
  where q  = 1 - p
        n' = fromIntegral n
        y  = n' `min` floorf (µ + σ * (d + γ * (d * d - 1) / 6) + 0.5)
          where µ  = n' * p
                σ  = sqrt (n' * p * q)
                d = D.quantile standard prob
                γ  = (q - p) / σ
        z  = cumulative d y
        search dy y z | z >= prob' = left y z
                      | otherwise  = right y
          where
            prob' = prob * (1 - 64 * m_epsilon)
            left y oldZ | y == 0 || z < prob' = (y, oldZ)
                        | otherwise           = left (max 0 y') z
                where z  = cumulative d y'
                      y' = y - dy
            right y | y' >= n' || z >= prob' = (y', z)
                    | otherwise              = right y'
                where z  = cumulative d y'
                      y' = y + dy
        narrow dy (y,z)
            | dy <= 1 || dy' <= n'/1e15 = y
            | otherwise                 = narrow dy' (search dy y z)
            where dy' = floorf (dy / 100)

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
