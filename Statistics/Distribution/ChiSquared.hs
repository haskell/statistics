{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.ChiSquared
-- Copyright : (c) 2010 Alexey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The chi-squared distribution. This is a continuous probability
-- distribution of sum of squares of k independent standard normal
-- distributions. It's commonly used in statistical tests
module Statistics.Distribution.ChiSquared (
          ChiSquared
        -- Constructors
        , chiSquared
        , chiSquaredNDF
        ) where

import Data.Typeable (Typeable)
import Statistics.Constants (m_huge)
import Statistics.Math      (incompleteGamma,invIncompleteGamma,logGamma)

import qualified Statistics.Distribution as D


-- | Chi-squared distribution
newtype ChiSquared = ChiSquared Int
                     deriving (Show,Typeable)

-- | Get number of degrees of freedom
chiSquaredNDF :: ChiSquared -> Int
chiSquaredNDF (ChiSquared ndf) = ndf
{-# INLINE chiSquaredNDF #-}

-- | Construct chi-squared distribution. Number of degrees of freedom
--   must be positive.
chiSquared :: Int -> ChiSquared
chiSquared n
  | n <= 0    = error $ 
     "Statistics.Distribution.ChiSquared.chiSquared: N.D.F. must be positive. Got " ++ show n
  | otherwise = ChiSquared n
{-# INLINE chiSquared #-}

instance D.Distribution ChiSquared where
  cumulative = cumulative

instance D.ContDistr ChiSquared where
  density  = density
  quantile = quantile

instance D.Mean ChiSquared where
    mean (ChiSquared ndf) = fromIntegral ndf
    {-# INLINE mean #-}

instance D.Variance ChiSquared where
    variance (ChiSquared ndf) = fromIntegral (2*ndf)
    {-# INLINE variance #-}

instance D.MaybeMean ChiSquared where
    maybeMean = Just . D.mean

instance D.MaybeVariance ChiSquared where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

cumulative :: ChiSquared -> Double -> Double
cumulative chi x
  | x <= 0    = 0
  | otherwise = incompleteGamma (ndf/2) (x/2)
  where
    ndf = fromIntegral $ chiSquaredNDF chi
{-# INLINE cumulative #-}

density :: ChiSquared -> Double -> Double
density chi x
  | x <= 0    = 0
  | otherwise = exp $ log x * (ndf2 - 1) - x2 - logGamma ndf2 - log 2 * ndf2
  where
    ndf  = fromIntegral $ chiSquaredNDF chi
    ndf2 = ndf/2
    x2   = x/2
{-# INLINE density #-}

quantile :: ChiSquared -> Double -> Double
quantile d@(ChiSquared ndf) p
  | p < 0 || p > 1 = 0/0
  | p == 0         = -1/0
  | p == 1         = 1/0
  | otherwise      = 2 * invIncompleteGamma (fromIntegral ndf / 2) p
{-# INLINE quantile #-}
