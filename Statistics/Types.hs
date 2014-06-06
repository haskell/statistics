-- |
-- Module    : Statistics.Types
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Types for working with statistics.

module Statistics.Types
    ( -- * Confidence level and intervals
      CL(..)
    , cl90
    , cl95
    , cl99
    , nSigma
    , getNSigma
      -- * Other
    , Estimator(..)
    , Sample
    , WeightedSample
    , Weights
    ) where

import Statistics.Types.Internal
import Statistics.Distribution
import Statistics.Distribution.Normal


----------------------------------------------------------------
-- Confidence level and estimates
----------------------------------------------------------------

-- | Confidence level. This data type serve two purposes:
--
--   1. In context of confidence intervals (CI) it should be
--      interpreted as probability that true value of parameter lies
--      OUTSIDE of interval. CI are constructed for /p/ close to 1 so
--      we store @1-p@ to avoid rounding errors when @p@ is very close
--      to 1. e.g. @CL 0.05@ corresponds to 95% CL.
--
--   2. In context of statistical tests it's p-value of test
--      significance.
newtype CL a = CL { getCL :: a }
               deriving (Show,Eq)

-- FIXME: is this right instance?
instance Ord a => Ord (CL a) where
  CL a <  CL b = a >  b
  CL a <= CL b = a >= b
  CL a >  CL b = a <  b
  CL a >= CL b = a <= b
  max (CL a) (CL b) = CL (min a b)
  min (CL a) (CL b) = CL (max a b)

cl90 :: Fractional a => CL a
cl90 = CL 0.10

cl95 :: Fractional a => CL a
cl95 = CL 0.05

cl99 :: Fractional a => CL a
cl99 = CL 0.01

-- | CL expressed in sigma. This is convention widely used in
--   experimental physics. N sigma confidence level corresponds to
--   probability within N sigma of normal distribution. Note that
--   there's no direct correspondence between standard deviation and
--   CL expressed in sigma.
nSigma :: Double -> CL Double
nSigma n
  | n > 0     = CL $ 2 * cumulative standard (-n)
  | otherwise = error "Statistics.Extra.Error.nSigma: non-positive number of sigma"

-- | Express confidence level in sigmas
getNSigma :: CL Double -> Double
getNSigma (CL p) = negate $ quantile standard (p / 2)
