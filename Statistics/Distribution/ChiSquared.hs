{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Numeric.SpecFunctions (incompleteGamma,invIncompleteGamma,logGamma)

import qualified Statistics.Distribution         as D
import qualified System.Random.MWC.Distributions as MWC


-- | Chi-squared distribution
newtype ChiSquared = ChiSquared Int
                     deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary ChiSquared

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

instance D.ContGen ChiSquared where
    genContVar (ChiSquared n) = MWC.chiSquare n


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
quantile (ChiSquared ndf) p
  | p == 0         = 0
  | p == 1         = 1/0
  | p > 0 && p < 1 = 2 * invIncompleteGamma (fromIntegral ndf / 2) p
  | otherwise      =
    error $ "Statistics.Distribution.ChiSquared.quantile: p must be in [0,1] range. Got: "++show p
{-# INLINE quantile #-}
