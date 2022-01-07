{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Uniform
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Variate distributed uniformly in the interval.
module Statistics.Distribution.Uniform
    (
      UniformDistribution
    -- * Constructors
    , uniformDistr
    , uniformDistrE
    -- ** Accessors
    , uniformA
    , uniformB
    ) where

import Control.Applicative
import Data.Aeson             (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary            (Binary(..))
import Data.Data              (Data, Typeable)
import System.Random.Stateful (uniformRM)
import GHC.Generics           (Generic)

import qualified Statistics.Distribution as D
import Statistics.Internal



-- | Uniform distribution from A to B
data UniformDistribution = UniformDistribution {
      uniformA :: {-# UNPACK #-} !Double -- ^ Low boundary of distribution
    , uniformB :: {-# UNPACK #-} !Double -- ^ Upper boundary of distribution
    } deriving (Eq, Typeable, Data, Generic)

instance Show UniformDistribution where
  showsPrec i (UniformDistribution a b) = defaultShow2 "uniformDistr" a b i
instance Read UniformDistribution where
  readPrec = defaultReadPrecM2 "uniformDistr" uniformDistrE

instance ToJSON UniformDistribution
instance FromJSON UniformDistribution where
  parseJSON (Object v) = do
    a <- v .: "uniformA"
    b <- v .: "uniformB"
    maybe (fail errMsg) return $ uniformDistrE a b
  parseJSON _ = empty

instance Binary UniformDistribution where
  put (UniformDistribution x y) = put x >> put y
  get = do
    a <- get
    b <- get
    maybe (fail errMsg) return $ uniformDistrE a b

-- | Create uniform distribution.
uniformDistr :: Double -> Double -> UniformDistribution
uniformDistr a b = maybe (error errMsg) id $ uniformDistrE a b

-- | Create uniform distribution.
uniformDistrE :: Double -> Double -> Maybe UniformDistribution
uniformDistrE a b
  | b < a     = Just $ UniformDistribution b a
  | a < b     = Just $ UniformDistribution a b
  | otherwise = Nothing
-- NOTE: failure is in default branch to guard against NaNs.

errMsg :: String
errMsg = "Statistics.Distribution.Uniform.uniform: wrong parameters"


instance D.Distribution UniformDistribution where
  cumulative (UniformDistribution a b) x
    | x < a     = 0
    | x > b     = 1
    | otherwise = (x - a) / (b - a)

instance D.ContDistr UniformDistribution where
  density (UniformDistribution a b) x
    | x < a     = 0
    | x > b     = 0
    | otherwise = 1 / (b - a)
  quantile (UniformDistribution a b) p
    | p >= 0 && p <= 1 = a + (b - a) * p
    | otherwise        =
      error $ "Statistics.Distribution.Uniform.quantile: p must be in [0,1] range. Got: "++show p
  complQuantile (UniformDistribution a b) p
    | p >= 0 && p <= 1 = b + (a - b) * p
    | otherwise        =
      error $ "Statistics.Distribution.Uniform.complQuantile: p must be in [0,1] range. Got: "++show p

instance D.Mean UniformDistribution where
  mean (UniformDistribution a b) = 0.5 * (a + b)

instance D.Variance UniformDistribution where
  -- NOTE: 1/sqrt 12 is not constant folded (#4101) so it's written as
  --       numerical constant. (Also FIXME!)
  stdDev   (UniformDistribution a b) = 0.2886751345948129 * (b - a)
  variance (UniformDistribution a b) = d * d / 12 where d = b - a

instance D.MaybeMean UniformDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance UniformDistribution where
    maybeStdDev   = Just . D.stdDev

instance D.Entropy UniformDistribution where
  entropy (UniformDistribution a b) = log (b - a)

instance D.MaybeEntropy UniformDistribution where
  maybeEntropy = Just . D.entropy

instance D.ContGen UniformDistribution where
    genContVar (UniformDistribution a b) = uniformRM (a,b)
