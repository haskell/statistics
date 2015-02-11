{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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
      -- ** As confidence level
    , confLevel
    , getCL
    , cl90
    , cl95
    , cl99
      -- ** As p-value
    , pValue
    , getPValue
      -- ** Normal approximation
    , nSigma
    , nSigma1
    , getNSigma
    , getNSigma1
      -- * Estimates and upper/lower limits
    , Estimate(..)
    , estimate
    , confidenceInterval
    , estimateInt
    , scaleEstimate
    , UpperLimit(..)
    , LowerLimit(..)
      -- * Other
    , Estimator(..)
    , Sample
    , WeightedSample
    , Weights
    ) where

import Control.DeepSeq
import Data.Aeson   (FromJSON, ToJSON)
import Data.Binary  (Binary)
import Data.Data    (Data,Typeable)
import GHC.Generics (Generic)

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
newtype CL a = CL a
               deriving (Show,Read,Eq, Typeable, Data, Generic)

instance Binary   a => Binary   (CL a)
instance FromJSON a => FromJSON (CL a)
instance ToJSON   a => ToJSON   (CL a)
instance NFData   a => NFData   (CL a) where
  rnf (CL a) = rnf a

-- | This instance is inverted relative to instance of underlying
--   type. In other words is larger if it describes greater confidence
--   or significance. This corresponds to smaller wrapped probability.
instance Ord a => Ord (CL a) where
  CL a <  CL b = a >  b
  CL a <= CL b = a >= b
  CL a >  CL b = a <  b
  CL a >= CL b = a <= b
  max (CL a) (CL b) = CL (min a b)
  min (CL a) (CL b) = CL (max a b)


-- | Construct confidence level.
--
--   > confLevel 0.90
--   >>> CL = 0.10
confLevel :: (Ord a, Num a) => a -> CL a
confLevel p
  | p >= 0 && p <= 1 = CL (1 - p)
  | otherwise        = error "Statistics.Types.confLevel: probability is out if [0,1] range"

-- | Construct p-value
pValue :: (Ord a, Num a) => a -> CL a
pValue p
  | p >= 0 && p <= 1 = CL p
  | otherwise        = error "Statistics.Types.pValue: probability is out if [0,1] range"

-- | Get p-value
getPValue :: CL a -> a
getPValue (CL p) = p

-- | Get confidence level
getCL :: (Ord a, Num a) => CL a -> a
getCL (CL p) = 1 - p

-- | 90% confidence level
cl90 :: Fractional a => CL a
cl90 = CL 0.10

-- | 95% confidence level
cl95 :: Fractional a => CL a
cl95 = CL 0.05

-- | 99% confidence level
cl99 :: Fractional a => CL a
cl99 = CL 0.01

-- | CL expressed in sigma. This is convention widely used in
--   experimental physics. N sigma confidence level corresponds to
--   probability within N sigma of normal distribution.
--
--   Note that this correspondence is for normal distribution. Other
--   distribution will have different dependency. Also experimental
--   distribution usually only approximately normal (especially at
--   extreme tails).
nSigma :: Double -> CL Double
nSigma n
  | n > 0     = CL $ 2 * cumulative standard (-n)
  | otherwise = error "Statistics.Extra.Error.nSigma: non-positive number of sigma"

-- | CL expressed in sigma for one-tail hypothesis. This correspond to
--   probability of obtaining value less than @N·σ@.
nSigma1 :: Double -> CL Double
nSigma1 n
  | n > 0     = CL $ cumulative standard (-n)
  | otherwise = error "Statistics.Extra.Error.nSigma1: non-positive number of sigma"

-- | Express confidence level in sigmas
getNSigma :: CL Double -> Double
getNSigma (CL p) = negate $ quantile standard (p / 2)

-- | Express confidence level in sigmas
getNSigma1 :: CL Double -> Double
getNSigma1 (CL p) = negate $ quantile standard p



----------------------------------------------------------------
-- Estimates and limits
----------------------------------------------------------------

-- | A point estimate and its confidence interval. Latter is
--   frequently referred to as error estimate.
data Estimate a = Estimate
    { estPoint           :: !a
      -- ^ Point estimate.
    , estErrors          :: !(a,a)
      -- ^ Estimate's error. They are given relative to central estimate.
    , estConfidenceLevel :: !(CL Double)
      -- ^ Confidence level of the confidence intervals.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary   a => Binary   (Estimate a)
instance FromJSON a => FromJSON (Estimate a)
instance ToJSON   a => ToJSON   (Estimate a)
instance NFData a => NFData (Estimate a) where
    rnf (Estimate x dx cl) = rnf x `seq` rnf dx `seq` rnf cl


-- | Construct estimate and check it for sanity
estimate :: (Ord a, Num a) => a -> (a,a) -> CL Double -> Estimate a
estimate x dx@(ldx,udx) p
  | ldx <= 0 && udx >= 0 = Estimate x dx p
  | otherwise            = error
      "Statistics.Types.estimate: invalid error values"

confidenceInterval :: Num a => Estimate a -> (a,a)
confidenceInterval Estimate{ estPoint = x, estErrors = (ldx,udx) }
  = (x + ldx, x + udx)

-- | Construct estimate from central estimate and confidence interval
estimateInt :: (Ord a, Num a) => a -> (a,a) -> CL Double -> Estimate a
estimateInt x (lo,hi) p
  | lo <= x && hi >= x = Estimate x (lo - x, hi - x) p
  | otherwise          = error
      "Statistics.Types.estimateInt: invalid error values"

-- | Multiply the point, lower bound, and upper bound in an 'Estimate'
scaleEstimate :: (Ord a, Num a) => a -> Estimate a -> Estimate a
scaleEstimate a (Estimate x (ldx,udx) cl)
  = Estimate (x * a) dx cl
  where
    dx | a > 0     = (ldx * a, udx * a)
       | otherwise = (udx * a, ldx * a)


-- | Upper limit. They are usually given for small non-negative values
--   when it's not possible detect difference from zero.
data UpperLimit a = UpperLimit
    { upperLimit        :: !a
      -- ^ Upper limit
    , ulConfidenceLevel :: !(CL Double)
      -- ^ Confidence level for which limit was calculated
    } deriving (Eq, Read, Show, Typeable, Data, Generic)


instance Binary   a => Binary   (UpperLimit a)
instance FromJSON a => FromJSON (UpperLimit a)
instance ToJSON   a => ToJSON   (UpperLimit a)
instance NFData   a => NFData   (UpperLimit a) where
    rnf (UpperLimit x cl) = rnf x `seq` rnf cl

-- | Lower limit. They are usually given for large quantities when
--   it's not possible to measure them. For example: proton half-life
data LowerLimit a = LowerLimit {
    lowerLimit        :: !a
    -- ^ Lower limit
  , llConfidenceLevel :: !(CL Double)
    -- ^ Confidence level for which limit was calculated
  } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary   a => Binary   (LowerLimit a)
instance FromJSON a => FromJSON (LowerLimit a)
instance ToJSON   a => ToJSON   (LowerLimit a)
instance NFData   a => NFData   (LowerLimit a) where
    rnf (LowerLimit x cl) = rnf x `seq` rnf cl
