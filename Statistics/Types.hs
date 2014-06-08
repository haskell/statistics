{-# LANGUAGE DeriveDataTypeable, DeriveGeneric,
  GeneralizedNewtypeDeriving #-}
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
    , confLevel
    , getCL
    , cl90
    , cl95
    , cl99
    , nSigma
    , getNSigma
      -- * Estimates and upper/lower limits
    , Estimate(..)
    , estimate
    , scaleEstimate
    , UpperLimit(..)
    , LowerLimit(..)
      -- * Other
    , Estimator(..)
    , Sample
    , WeightedSample
    , Weights
    ) where

import Control.Applicative
import Control.DeepSeq
import Data.Binary (Binary,put,get)
import Data.Data   (Data,Typeable)
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
newtype CL a = CL { unCL :: a }
               deriving (Show,Read,Eq, Typeable, Data, Generic, Binary, NFData)

-- FIXME: is this right instance?
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
--   >>> CL { unCL = 0.10 }
confLevel :: (Ord a, Num a) => a -> CL a
confLevel p
  | p > 0 && p < 1 = CL (1 - p)
  | otherwise      = error "Statistics.Types.confLevel: probability is out if (0,1) range"

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
    , estConfidenceLevel :: !(CL a)
      -- ^ Confidence level of the confidence intervals.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Construct estimate and check it for sanity
estimate :: (Ord a, Num a) => a -> (a,a) -> CL a -> Estimate a
estimate x dx@(ldx,udx) p
  | ldx <= 0 && udx >= 0 = Estimate x dx p
  | otherwise            = error
      "Statistics.Types.estimate: invalid error values"

-- | Multiply the point, lower bound, and upper bound in an 'Estimate'
scaleEstimate :: (Ord a, Num a) => a -> Estimate a -> Estimate a
scaleEstimate a (Estimate x (ldx,udx) cl)
  = Estimate (x * a) dx cl
  where
    dx | a > 0     = (ldx * a, udx * a)
       | otherwise = (udx * a, ldx * a)

instance Binary a => Binary (Estimate a) where
    put (Estimate x dx cl) = put x >> put dx >> put cl
    get = Estimate <$> get <*> get <*> get
instance NFData a => NFData (Estimate a) where
    rnf (Estimate x dx cl) = rnf x `seq` rnf dx `seq` rnf cl

-- | Upper limit. They are usually given for small non-negative values
--   when it's not possible detect difference from zero.
data UpperLimit a = UpperLimit
    { upperLimit        :: !a
      -- ^ Upper limit
    , ulConfidenceLevel :: !(CL a)
      -- ^ Confidence level for which limit was calculated
    } deriving (Eq, Read, Show, Typeable, Data, Generic)


instance Binary a => Binary (UpperLimit a) where
    put (UpperLimit x cl) = put x >> put cl
    get = UpperLimit <$> get <*> get
instance NFData a => NFData (UpperLimit a) where
    rnf (UpperLimit x cl) = rnf x `seq` rnf cl

-- | Lower limit. They are usually given for large quantities when
--   it's not possible to measure them. For example: proton half-life
data LowerLimit a = LowerLimit {
    lowerLimit        :: !a
    -- ^ Lower limit
  , llConfidenceLevel :: !(CL a)
    -- ^ Confidence level for which limit was calculated
  } deriving (Eq, Read, Show, Typeable, Data, Generic)


instance Binary a => Binary (LowerLimit a) where
    put (LowerLimit x cl) = put x >> put cl
    get = LowerLimit <$> get <*> get
instance NFData a => NFData (LowerLimit a) where
    rnf (LowerLimit x cl) = rnf x `seq` rnf cl
