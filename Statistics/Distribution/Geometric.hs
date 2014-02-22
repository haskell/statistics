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
    , geometric0
    -- ** Accessors
    , gdSuccess
    , gdSuccess0
    ) where

import Control.Monad  (liftM)
import Data.Binary    (Binary)
import Data.Data      (Data, Typeable)
import GHC.Generics   (Generic)
import Numeric.MathFunctions.Constants(m_pos_inf,m_neg_inf)
import qualified Statistics.Distribution         as D
import qualified System.Random.MWC.Distributions as MWC
import Data.Binary (put, get)
import Control.Applicative ((<$>))

----------------------------------------------------------------
-- Distribution over [1..]

newtype GeometricDistribution = GD {
      gdSuccess :: Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary GeometricDistribution where
    get = GD <$> get
    put (GD x) = put x

instance D.Distribution GeometricDistribution where
    cumulative = cumulative

instance D.DiscreteDistr GeometricDistribution where
    probability (GD s) n
      | n < 1     = 0
      | otherwise = s * (1-s) ** (fromIntegral n - 1)
    logProbability (GD s) n
       | n < 1     = m_neg_inf
       | otherwise = log s + log (1-s) * (fromIntegral n - 1)


instance D.Mean GeometricDistribution where
    mean (GD s) = 1 / s
    {-# INLINE mean #-}

instance D.Variance GeometricDistribution where
    variance (GD s) = (1 - s) / (s * s)
    {-# INLINE variance #-}

instance D.MaybeMean GeometricDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance GeometricDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy GeometricDistribution where
  entropy (GD s)
    | s == 0 = m_pos_inf
    | s == 1 = 0
    | otherwise = negate $ (s * log s + (1-s) * log (1-s)) / s

instance D.MaybeEntropy GeometricDistribution where
  maybeEntropy = Just . D.entropy

instance D.DiscreteGen GeometricDistribution where
  genDiscreteVar (GD s) g = MWC.geometric1 s g
  {-# INLINE genDiscreteVar #-}
instance D.ContGen GeometricDistribution where
  genContVar d g = fromIntegral `liftM` D.genDiscreteVar d g
  {-# INLINE genContVar #-}

-- | Create geometric distribution.
geometric :: Double                -- ^ Success rate
          -> GeometricDistribution
geometric x
  | x >= 0 && x <= 1 = GD x
  | otherwise        =
    error $ "Statistics.Distribution.Geometric.geometric: probability must be in [0,1] range. Got " ++ show x
{-# INLINE geometric #-}

cumulative :: GeometricDistribution -> Double -> Double
cumulative (GD s) x
  | x < 1        = 0
  | isInfinite x = 1
  | isNaN      x = error "Statistics.Distribution.Geometric.cumulative: NaN input"
  | otherwise    = 1 - (1-s) ^ (floor x :: Int)
{-# INLINE cumulative #-}


----------------------------------------------------------------
-- Distribution over [0..]

newtype GeometricDistribution0 = GD0 {
      gdSuccess0 :: Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary GeometricDistribution0 where
    get = GD0 <$> get
    put (GD0 x) = put x

instance D.Distribution GeometricDistribution0 where
    cumulative (GD0 s) x = cumulative (GD s) (x + 1)

instance D.DiscreteDistr GeometricDistribution0 where
    probability    (GD0 s) n = D.probability    (GD s) (n + 1)
    logProbability (GD0 s) n = D.logProbability (GD s) (n + 1)

instance D.Mean GeometricDistribution0 where
    mean (GD0 s) = 1 / s - 1
    {-# INLINE mean #-}

instance D.Variance GeometricDistribution0 where
    variance (GD0 s) = D.variance (GD s)
    {-# INLINE variance #-}

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
  {-# INLINE genDiscreteVar #-}
instance D.ContGen GeometricDistribution0 where
  genContVar d g = fromIntegral `liftM` D.genDiscreteVar d g
  {-# INLINE genContVar #-}

-- | Create geometric distribution.
geometric0 :: Double                -- ^ Success rate
           -> GeometricDistribution0
geometric0 x
  | x >= 0 && x <= 1 = GD0 x
  | otherwise        =
    error $ "Statistics.Distribution.Geometric.geometric: probability must be in [0,1] range. Got " ++ show x
{-# INLINE geometric0 #-}
