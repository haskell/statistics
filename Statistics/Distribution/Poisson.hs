{-# LANGUAGE OverloadedStrings #-}
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
    , poissonE
    -- * Accessors
    , poissonLambda
    -- * References
    -- $references
    ) where

import Control.Applicative
import Data.Aeson           (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary          (Binary(..))
import Data.Data            (Data, Typeable)
import GHC.Generics         (Generic)
import Numeric.SpecFunctions (incompleteGamma,logFactorial)
import Numeric.MathFunctions.Constants (m_neg_inf)

import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Poisson.Internal as I
import Statistics.Internal



newtype PoissonDistribution = PD {
      poissonLambda :: Double
    } deriving (Eq, Typeable, Data, Generic)

instance Show PoissonDistribution where
  showsPrec i (PD l) = defaultShow1 "poisson" l i
instance Read PoissonDistribution where
  readPrec = defaultReadPrecM1 "poisson" poissonE

instance ToJSON PoissonDistribution
instance FromJSON PoissonDistribution where
  parseJSON (Object v) = do
    l <- v .: "poissonLambda"
    maybe (fail $ errMsg l) return $ poissonE l
  parseJSON _ = empty

instance Binary PoissonDistribution where
  put = put . poissonLambda
  get = do
    l <- get
    maybe (fail $ errMsg l) return $ poissonE l

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
poisson l = maybe (error $ errMsg l) id $ poissonE l

-- | Create Poisson distribution.
poissonE :: Double -> Maybe PoissonDistribution
poissonE l
  | l >=  0   = Just (PD l)
  | otherwise = Nothing

errMsg :: Double -> String
errMsg l = "Statistics.Distribution.Poisson.poisson: lambda must be non-negative. Got "
        ++ show l


-- $references
--
-- * Loader, C. (2000) Fast and Accurate Computation of Binomial
--   Probabilities. <http://projects.scipy.org/scipy/raw-attachment/ticket/620/loader2000Fast.pdf>
-- * Adell, J., Lekuona, A., and Yu, Y. (2010) Sharp Bounds on the
--   Entropy of the Poisson Law and Related Quantities
--   <http://arxiv.org/pdf/1001.2897.pdf>
