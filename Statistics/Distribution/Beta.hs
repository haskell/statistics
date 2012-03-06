{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Statistics.Distribution.Beta
-- Copyright   :  (C) 2012 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  DeriveDataTypeable
--
----------------------------------------------------------------------------
module Statistics.Distribution.Beta
  ( BetaDistribution(..)
  , betaDistr
  ) where

import Numeric.SpecFunctions
import Numeric.MathFunctions.Constants (m_NaN)
import qualified Statistics.Distribution as D
import Data.Typeable

data BetaDistribution = BD
 { bdAlpha :: {-# UNPACK #-} !Double
 , bdBeta  :: {-# UNPACK #-} !Double
 } deriving (Eq,Read,Show,Typeable)

betaDistr :: Double -> Double -> BetaDistribution
betaDistr a b
  | a < 0 = error $ msg ++ "alpha must be positive. Got " ++ show a
  | b < 0 = error $ msg ++ "beta must be positive. Got " ++ show b
  | otherwise = BD a b
  where msg = "Statistics.Distribution.Beta.betaDistr: "
{-# INLINE betaDistr #-}

instance D.Distribution BetaDistribution where
  cumulative (BD a b) x
    | x <= 0 = 0
    | otherwise = incompleteBeta a b x
  {-# INLINE cumulative #-}

instance D.Mean BetaDistribution where
  mean (BD a b) = a / (a + b)
  {-# INLINE mean #-}

instance D.MaybeMean BetaDistribution where
  maybeMean = Just . D.mean
  {-# INLINE maybeMean #-}

instance D.Variance BetaDistribution where
  variance (BD a b) = a*b / (apb*apb*(apb+1))
    where apb = a + b
  {-# INLINE variance #-}

-- invert a monotone function
invertMono :: (Double -> Double) -> Double -> Double -> Double -> Double
invertMono f l0 h0 b = go l0 h0 where
  go l h
    | h - l < epsilon = m
    | otherwise = case compare (f m) b of
      LT -> go m h
      EQ -> m
      GT -> go l m
    where m = l + (h-l)/2
          epsilon = 1e-12
{-# INLINE invertMono #-}

instance D.ContDistr BetaDistribution where
  density (BD a b) x
   | a <= 0 || b <= 0 = m_NaN
   | x <= 0 = 0
   | x >= 1 = 0
   | otherwise = exp $ (a-1)*log x + (b-1)*log (1-x) - logBeta a b
  {-# INLINE density #-}

  quantile d p
    | p < 0 = error $ "probability must be positive. Got " ++ show p
    | p > 1 = error $ "probability must be less than 1. Got " ++ show p
    | otherwise = invertMono (D.cumulative d) 0 1 p
  {-# INLINE quantile #-}

instance D.MaybeVariance BetaDistribution where
  maybeVariance = Just . D.variance
  {-# INLINE maybeVariance #-}

-- TODO: D.ContGen for rbeta
