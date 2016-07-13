{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ( -- * Confidence level
      CL
      -- ** Accessors
    , confLevel
    , getPValue
      -- ** Constructors
    , mkConfLevel
    , mkConfLevelE
    , clFromPVal
    , clFromPValE
    , asCL
      -- ** Constants and conversion to nσ
    , cl90
    , cl95
    , cl99
      -- *** Normal approximation
    , nSigma
    , nSigma1
    , getNSigma
    , getNSigma1
      -- * p-value
    , PValue
      -- ** Accessors
    , pValue
      -- ** Constructors
    , mkPValue
    , mkPValueE
    , asPValue
      -- * Estimates and upper/lower limits
    , Estimate(..)
    , NormalErr(..)
    , ConfInt(..)
    , UpperLimit(..)
    , LowerLimit(..)
      -- ** Constructors
    , estimateNormErr
    , (±)
    , estimateFromInterval
    , estimateFromErr
      -- ** Accessors
    , confidenceInterval
    , asymErrors
    , Scale(..)
      -- * Other
    , Sample
    , WeightedSample
    , Weights
    ) where

import Control.Monad                ((<=<))
import Control.DeepSeq              (NFData(..))
import Data.Aeson                   (FromJSON(..), ToJSON)
import Data.Binary                  (Binary(..))
import Data.Data                    (Data,Typeable)
import Data.Maybe                   (fromMaybe)
import Data.Vector.Unboxed          (Unbox)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Generics (Generic)

import Statistics.Internal
import Statistics.Types.Internal
import Statistics.Distribution
import Statistics.Distribution.Normal


----------------------------------------------------------------
-- Data type for confidence level
----------------------------------------------------------------

-- |
-- Confidence level. In context of confidence intervals (CI) it's
-- represented as probability that true value of parameter lies
-- OUTSIDE of interval. CI are constructed for /p/ close to 1 so we
-- store @1-p@ to avoid rounding errors when @p@ is very close to
-- 1. e.g. 95% CL represented as @CL 0.05@.
newtype CL a = CL a
               deriving (Eq, Typeable, Data, Generic)

instance Show a => Show (CL a) where
  showsPrec n (CL p) = defaultShow1 "clFromPVal" p n
instance (Num a, Ord a, Read a) => Read (CL a) where
  readPrec = defaultReadPrecM1 "clFromPVal" clFromPValE

instance (Binary a, Num a, Ord a) => Binary (CL a) where
  put (CL p) = put p
  get        = maybe (fail errMkCL) return . clFromPValE =<< get

instance (ToJSON a)                 => ToJSON   (CL a)
instance (FromJSON a, Num a, Ord a) => FromJSON (CL a) where
  parseJSON = maybe (fail errMkCL) return . clFromPValE <=< parseJSON

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


-- | Create confidence level. Will throw exception if parameter is out
--   of [0,1] range
mkConfLevel :: (Ord a, Num a) => a -> CL a
mkConfLevel
  = fromMaybe (error "Statistics.Types.mkConfLevel: probability is out if [0,1] range")
  . mkConfLevelE

-- | Create confidence level.
mkConfLevelE :: (Ord a, Num a) => a -> Maybe (CL a)
mkConfLevelE p
  | p >= 0 && p <= 1 = Just $ CL (1 - p)
  | otherwise        = Nothing

-- | Create confidence level. Will throw exception if parameter is out
--   of [0,1] range
clFromPVal :: (Ord a, Num a) => a -> CL a
clFromPVal = fromMaybe (error errMkCL) . clFromPValE

-- | Create confidence level.
clFromPValE :: (Ord a, Num a) => a -> Maybe (CL a)
clFromPValE p
  | p >= 0 && p <= 1 = Just $ CL p
  | otherwise        = Nothing

errMkCL :: String
errMkCL = "Statistics.Types.mkPValCL: probability is out if [0,1] range"

-- |
-- Convert p-value to confidence level. It's interpreted as
-- probability of hypothesis being wrong.
asCL :: PValue a -> CL a
asCL (PValue p) = CL p


-- | Get confidence level. This function is subject to rounding
--   errors. If @1 - CL@ is needed use 'getPValue' instead
confLevel :: (Num a) => CL a -> a
confLevel (CL p) = 1 - p

-- | Get probability of hypothesis being false.
getPValue :: CL a -> a
getPValue (CL p) = p



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
-- Data type for p-value
----------------------------------------------------------------

-- | Newtype wrapper for p-value
newtype PValue a = PValue a
               deriving (Eq,Ord, Typeable, Data, Generic)

instance Show a => Show (PValue a) where
  showsPrec n (PValue p) = defaultShow1 "mkPValue" p n
instance (Num a, Ord a, Read a) => Read (PValue a) where
  readPrec = defaultReadPrecM1 "mkPValue" mkPValueE

instance (Binary a, Num a, Ord a) => Binary (PValue a) where
  put (PValue p) = put p
  get            = maybe (fail errMkPValue) return . mkPValueE =<< get

instance (ToJSON a)                 => ToJSON   (PValue a)
instance (FromJSON a, Num a, Ord a) => FromJSON (PValue a) where
  parseJSON = maybe (fail errMkPValue) return . mkPValueE <=< parseJSON

instance NFData a => NFData (PValue a) where
  rnf (PValue a) = rnf a


-- | Construct PValue. Throws error if argument is out of [0,1] range
mkPValue :: (Ord a, Num a) => a -> PValue a
mkPValue = fromMaybe (error errMkPValue) . mkPValueE

-- | Construct PValue.
mkPValueE :: (Ord a, Num a) => a -> Maybe (PValue a)
mkPValueE p
  | p >= 0 && p <= 1 = Just $ PValue p
  | otherwise        = Nothing

-- | Get p-value
pValue :: PValue a -> a
pValue (PValue p) = p

-- | Convert confidence level to p-value. It returns probability of
--   hypothesis being false.
asPValue :: CL a -> PValue a
asPValue (CL p) = PValue p

errMkPValue :: String
errMkPValue = "Statistics.Types.mkPValue: probability is out if [0,1] range"



----------------------------------------------------------------
-- Point estimates
----------------------------------------------------------------

-- | A point estimate and its confidence interval. Latter is
--   frequently referred to as error estimate.
data Estimate e a = Estimate
    { estPoint           :: !a
      -- ^ Point estimate.
    , estError           :: !(e a)
      -- ^ Confidence error estimate. It's parametrized by
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance (Binary   (e a), Binary   a) => Binary   (Estimate e a)
instance (FromJSON (e a), FromJSON a) => FromJSON (Estimate e a)
instance (ToJSON   (e a), ToJSON   a) => ToJSON   (Estimate e a)
instance (NFData   (e a), NFData   a) => NFData   (Estimate e a) where
    rnf (Estimate x dx) = rnf x `seq` rnf dx



-- | Normal errors. They are stored as 1σ errors. Since we can
-- recalculate them to any confidence level if needed we don't store
-- it.
newtype NormalErr a = NormalErr
  { normalError :: a
  }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary   a => Binary   (NormalErr a)
instance FromJSON a => FromJSON (NormalErr a)
instance ToJSON   a => ToJSON   (NormalErr a)
instance NFData   a => NFData   (NormalErr a) where
    rnf (NormalErr x) = rnf x


-- | Confidence interval. It assumes that confidence interval forms
--   single interval and isn't set of disjoint intervals.
data ConfInt a = ConfInt
  { confIntLDX :: !a
    -- ^ Lower error estimate, or distance between point estimate and
    --   lower bound of confidence interval.
  , confIntUDX :: !a
    -- ^ Upper error estimate, or distance between point estimate and
    --   upper bound of confidence interval.
  , confIntCL  :: !(CL Double)
    -- ^ Confidence level corresponding to given confidence interval.
  }
  deriving (Read,Show,Eq,Typeable,Data,Generic)

instance Binary   a => Binary   (ConfInt a)
instance FromJSON a => FromJSON (ConfInt a)
instance ToJSON   a => ToJSON   (ConfInt a)
instance NFData   a => NFData   (ConfInt a) where
    rnf (ConfInt x y _) = rnf x `seq` rnf y



----------------------------------------
-- Constructors

-- | Create estimate with normal errors
estimateNormErr :: a            -- ^ Point estimate
                -> a            -- ^ 1σ error
                -> Estimate NormalErr a
estimateNormErr x dx = Estimate x (NormalErr dx)

-- | Synonym for 'estimateNormErr'
(±) :: a      -- ^ Point estimate
    -> a      -- ^ 1σ error
    -> Estimate NormalErr a
(±) = estimateNormErr

-- | Create estimate with asymmetric error.
estimateFromErr
  :: a                     -- ^ Central estimate
  -> (a,a)                 -- ^ Lower and upper errors. Both should be
                           --   positive but it's not checked.
  -> CL Double             -- ^ Confidence level for interval
  -> Estimate ConfInt a
estimateFromErr x (ldx,udx) cl = Estimate x (ConfInt ldx udx cl)

estimateFromInterval
  :: Num a
  => a                     -- ^ Point estimate. Should lie within
                           --   interval but it's not checked.
  -> (a,a)                 -- ^ Lower and upper bounds of interval
  -> CL Double             -- ^ Confidence level for interval
  -> Estimate ConfInt a
estimateFromInterval x (lx,ux) cl
  = Estimate x (ConfInt (x-lx) (ux-x) cl)


----------------------------------------
-- Accessors

-- | Get confidence interval
confidenceInterval :: Num a => Estimate ConfInt a -> (a,a)
confidenceInterval (Estimate x (ConfInt ldx udx _))
  = (x - ldx, x + udx)

-- | Get asymmetric errors
asymErrors :: Estimate ConfInt a -> (a,a)
asymErrors (Estimate _ (ConfInt ldx udx _)) = (ldx,udx)



-- | Data types which could be multiplied by constant.
class Scale e where
  scale :: (Ord a, Num a) => a -> e a -> e a

instance Scale NormalErr where
  scale a (NormalErr e) = NormalErr (abs a * e)

instance Scale ConfInt where
  scale a (ConfInt l u cl) | a >= 0    = ConfInt  (a*l)  (a*u) cl
                           | otherwise = ConfInt (-a*u) (-a*l) cl

instance Scale e => Scale (Estimate e) where
  scale a (Estimate x dx) = Estimate (a*x) (scale a dx)



----------------------------------------------------------------
-- Upper/lower limit
----------------------------------------------------------------

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


----------------------------------------------------------------
-- Deriving unbox instances
----------------------------------------------------------------

derivingUnbox "CL"
  [t| forall a. Unbox a => CL a -> a |]
  [| \(CL a) -> a |]
  [| CL           |]

derivingUnbox "PValue"
  [t| forall a. Unbox a => PValue a -> a |]
  [| \(PValue a) -> a |]
  [| PValue           |]

derivingUnbox "Estimate"
  [t| forall a e. (Unbox a, Unbox (e a)) => Estimate e a -> (a, e a) |]
  [| \(Estimate x dx) -> (x,dx) |]
  [| \(x,dx) -> (Estimate x dx) |]

derivingUnbox "NormalErr"
  [t| forall a. Unbox a => NormalErr a -> a |]
  [| \(NormalErr a) -> a |]
  [| NormalErr           |]

derivingUnbox "ConfInt"
  [t| forall a. Unbox a => ConfInt a -> (a, a, CL Double) |]
  [| \(ConfInt a b c) -> (a,b,c) |]
  [| \(a,b,c) -> ConfInt a b c   |]

derivingUnbox "UpperLimit"
  [t| forall a. Unbox a => UpperLimit a -> (a, CL Double) |]
  [| \(UpperLimit a b) -> (a,b) |]
  [| \(a,b) -> UpperLimit a b   |]

derivingUnbox "LowerLimit"
  [t| forall a. Unbox a => LowerLimit a -> (a, CL Double) |]
  [| \(LowerLimit a b) -> (a,b) |]
  [| \(a,b) -> LowerLimit a b   |]
