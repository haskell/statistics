{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Geometric
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Geometric distribution. There are two variants of
-- distribution. First is the probability distribution of the number
-- of Bernoulli trials needed to get one success, supported on the set
-- [1,2..] ('GeometricDistribution'). Sometimes it's referred to as
-- the /shifted/ geometric distribution to distinguish from another
-- one.
--
-- Second variant is probability distribution of the number of
-- failures before first success, defined over the set [0,1..]
-- ('GeometricDistribution0').
module Statistics.Distribution.Geometric
    (
      GeometricDistribution
    , GeometricDistribution0
    -- * Constructors
    , geometric
    , geometricE
    , geometric0
    , geometric0E
    -- ** Accessors
    , gdSuccess
    , gdSuccess0
    ) where

import Control.Applicative
import Control.Monad       (liftM)
import Data.Aeson          (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary         (Binary(..))
import Data.Data           (Data, Typeable)
import GHC.Generics        (Generic)
import Numeric.MathFunctions.Constants (m_neg_inf)
import Numeric.SpecFunctions           (log1p,expm1)
import qualified System.Random.MWC.Distributions as MWC

import qualified Statistics.Distribution as D
import Statistics.Internal



----------------------------------------------------------------

-- | Distribution over [1..]
newtype GeometricDistribution = GD {
      gdSuccess :: Double
    } deriving (Eq, Typeable, Data, Generic)

instance Show GeometricDistribution where
  showsPrec i (GD x) = defaultShow1 "geometric" x i
instance Read GeometricDistribution where
  readPrec = defaultReadPrecM1 "geometric" geometricE

instance ToJSON GeometricDistribution
instance FromJSON GeometricDistribution where
  parseJSON (Object v) = do
    x <- v .: "gdSuccess"
    maybe (fail $ errMsg x) return  $ geometricE x
  parseJSON _ = empty

instance Binary GeometricDistribution where
  put (GD x) = put x
  get = do
    x <- get
    maybe (fail $ errMsg x) return  $ geometricE x


instance D.Distribution GeometricDistribution where
    cumulative      = cumulative
    complCumulative = complCumulative

instance D.DiscreteDistr GeometricDistribution where
    probability (GD s) n
      | n < 1     = 0
      | s >= 0.5  = s * (1 - s)^(n - 1)
      | otherwise = s * (exp $ log1p (-s) * (fromIntegral n - 1))
    logProbability (GD s) n
       | n < 1     = m_neg_inf
       | otherwise = log s + log1p (-s) * (fromIntegral n - 1)


instance D.Mean GeometricDistribution where
    mean (GD s) = 1 / s

instance D.Variance GeometricDistribution where
    variance (GD s) = (1 - s) / (s * s)

instance D.MaybeMean GeometricDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance GeometricDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy GeometricDistribution where
  entropy (GD s)
    | s == 1 = 0
    | otherwise = -(s * log s + (1-s) * log1p (-s)) / s

instance D.MaybeEntropy GeometricDistribution where
  maybeEntropy = Just . D.entropy

instance D.DiscreteGen GeometricDistribution where
  genDiscreteVar (GD s) g = MWC.geometric1 s g

instance D.ContGen GeometricDistribution where
  genContVar d g = fromIntegral `liftM` D.genDiscreteVar d g

cumulative :: GeometricDistribution -> Double -> Double
cumulative (GD s) x
  | x < 1        = 0
  | isInfinite x = 1
  | isNaN      x = error "Statistics.Distribution.Geometric.cumulative: NaN input"
  | s >= 0.5     = 1 - (1 - s)^k
  | otherwise    = negate $ expm1 $ fromIntegral k * log1p (-s)
    where k = floor x :: Int

complCumulative :: GeometricDistribution -> Double -> Double
complCumulative (GD s) x
  | x < 1        = 1
  | isInfinite x = 0
  | isNaN      x = error "Statistics.Distribution.Geometric.complCumulative: NaN input"
  | s >= 0.5     = (1 - s)^k
  | otherwise    = exp $ fromIntegral k * log1p (-s)
    where k = floor x :: Int


-- | Create geometric distribution.
geometric :: Double                -- ^ Success rate
          -> GeometricDistribution
geometric x = maybe (error $ errMsg x) id $ geometricE x

-- | Create geometric distribution.
geometricE :: Double                -- ^ Success rate
           -> Maybe GeometricDistribution
geometricE x
  | x > 0 && x <= 1  = Just (GD x)
  | otherwise        = Nothing

errMsg :: Double -> String
errMsg x = "Statistics.Distribution.Geometric.geometric: probability must be in (0,1] range. Got " ++ show x


----------------------------------------------------------------

-- | Distribution over [0..]
newtype GeometricDistribution0 = GD0 {
      gdSuccess0 :: Double
    } deriving (Eq, Typeable, Data, Generic)

instance Show GeometricDistribution0 where
  showsPrec i (GD0 x) = defaultShow1 "geometric0" x i
instance Read GeometricDistribution0 where
  readPrec = defaultReadPrecM1 "geometric0" geometric0E

instance ToJSON GeometricDistribution0
instance FromJSON GeometricDistribution0 where
  parseJSON (Object v) = do
    x <- v .: "gdSuccess0"
    maybe (fail $ errMsg x) return  $ geometric0E x
  parseJSON _ = empty

instance Binary GeometricDistribution0 where
  put (GD0 x) = put x
  get = do
    x <- get
    maybe (fail $ errMsg x) return  $ geometric0E x


instance D.Distribution GeometricDistribution0 where
    cumulative      (GD0 s) x = cumulative      (GD s) (x + 1)
    complCumulative (GD0 s) x = complCumulative (GD s) (x + 1)

instance D.DiscreteDistr GeometricDistribution0 where
    probability    (GD0 s) n = D.probability    (GD s) (n + 1)
    logProbability (GD0 s) n = D.logProbability (GD s) (n + 1)

instance D.Mean GeometricDistribution0 where
    mean (GD0 s) = 1 / s - 1

instance D.Variance GeometricDistribution0 where
    variance (GD0 s) = D.variance (GD s)

instance D.MaybeMean GeometricDistribution0 where
    maybeMean = Just . D.mean

instance D.MaybeVariance GeometricDistribution0 where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy GeometricDistribution0 where
  entropy (GD0 s) = D.entropy (GD s)

instance D.MaybeEntropy GeometricDistribution0 where
  maybeEntropy = Just . D.entropy

instance D.DiscreteGen GeometricDistribution0 where
  genDiscreteVar (GD0 s) g = MWC.geometric0 s g

instance D.ContGen GeometricDistribution0 where
  genContVar d g = fromIntegral `liftM` D.genDiscreteVar d g


-- | Create geometric distribution.
geometric0 :: Double                -- ^ Success rate
           -> GeometricDistribution0
geometric0 x = maybe (error $ errMsg0 x) id $ geometric0E x

-- | Create geometric distribution.
geometric0E :: Double                -- ^ Success rate
            -> Maybe GeometricDistribution0
geometric0E x
  | x > 0 && x <= 1  = Just (GD0 x)
  | otherwise        = Nothing

errMsg0 :: Double -> String
errMsg0 x = "Statistics.Distribution.Geometric.geometric0: probability must be in (0,1] range. Got " ++ show x
