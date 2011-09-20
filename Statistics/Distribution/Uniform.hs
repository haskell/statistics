{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Normal
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Variate distributed uniformly in the interval.
module Statistics.Distribution.Uniform (
    UniformDistribution
  , uniformDistr
  ) where

import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D

-- | Uniform distribution
data UniformDistribution = UniformDistribution {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                           deriving (Eq,Show,Read,Typeable)

-- | Create uniform distribution.
uniformDistr :: Double -> Double -> UniformDistribution
uniformDistr a b
  | b < a     = uniformDistr b a
  | a < b     = UniformDistribution a b
  | otherwise = error "Statistics.Distribution.Uniform.uniform: wrong parameters"
-- NOTE: failure is in default branch to guard againist NaNs.
                
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
    | p < 0 || p > 1 = 0/0
    | otherwise      = a + (b - a) * p

instance D.Mean UniformDistribution where
  mean (UniformDistribution a b) = 0.5 * (a + b)

instance D.Variance UniformDistribution where
  -- NOTE: 1/sqrt 12 is not constant folded (#4101) so it's written as
  --       numerical constant. (Also FIXME!)
  stdDev   (UniformDistribution a b) = 0.2886751345948129 * (b - a)
  variance (UniformDistribution a b) = d * d / 12 where d = b - a
