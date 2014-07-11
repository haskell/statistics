{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Poisson
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Poisson distribution.  This is the discrete probability
-- distribution of a number of events occurring in a fixed interval if
-- these events occur with a known average rate, and occur
-- independently from each other within that interval.

module Statistics.Distribution.Poisson
    (
      PoissonDistribution
    -- * Constructors
    , poisson
    -- * Accessors
    , poissonLambda
    -- * References
    -- $references
    ) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Poisson.Internal as I
import Numeric.SpecFunctions (incompleteGamma,logFactorial)
import Numeric.MathFunctions.Constants (m_neg_inf)
import Data.Binary (put, get)


newtype PoissonDistribution = PD {
      poissonLambda :: Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary PoissonDistribution where
    get = fmap PD get
    put = put . poissonLambda

instance D.Distribution PoissonDistribution where
    cumulative (PD lambda) x
      | x < 0        = 0
      | isInfinite x = 1
      | isNaN      x = error "Statistics.Distribution.Poisson.cumulative: NaN input"
      | otherwise    = 1 - incompleteGamma (fromIntegral (floor x + 1 :: Int)) lambda

instance D.DiscreteDistr PoissonDistribution where
    probability (PD lambda) x = I.probability lambda (fromIntegral x)
    logProbability (PD lambda) i
      | i < 0     = m_neg_inf
      | otherwise = log lambda * fromIntegral i - logFactorial i - lambda

instance D.Variance PoissonDistribution where
    variance = poissonLambda

instance D.Mean PoissonDistribution where
    mean = poissonLambda

instance D.MaybeMean PoissonDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance PoissonDistribution where
    maybeStdDev   = Just . D.stdDev

instance D.Entropy PoissonDistribution where
  entropy (PD lambda) = I.poissonEntropy lambda

instance D.MaybeEntropy PoissonDistribution where
  maybeEntropy = Just . D.entropy

-- | Create Poisson distribution.
poisson :: Double -> PoissonDistribution
poisson l
  | l >=  0   = PD l
  | otherwise = error $
    "Statistics.Distribution.Poisson.poisson: lambda must be non-negative. Got "
    ++ show l

-- $references
--
-- * Loader, C. (2000) Fast and Accurate Computation of Binomial
--   Probabilities. <http://projects.scipy.org/scipy/raw-attachment/ticket/620/loader2000Fast.pdf>
-- * Adell, J., Lekuona, A., and Yu, Y. (2010) Sharp Bounds on the
--   Entropy of the Poisson Law and Related Quantities
--   <http://arxiv.org/pdf/1001.2897.pdf>
