{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.FDistribution
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Fisher F distribution
module Statistics.Distribution.FDistribution (
    FDistribution
  , fDistribution
  , fDistributionNDF1
  , fDistributionNDF2
  ) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Statistics.Distribution as D
import Numeric.SpecFunctions (logBeta, incompleteBeta, invIncompleteBeta)



-- | F distribution
data FDistribution = F { fDistributionNDF1 :: {-# UNPACK #-} !Double
                       , fDistributionNDF2 :: {-# UNPACK #-} !Double
                       , _pdfFactor        :: {-# UNPACK #-} !Double
                       }
                   deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Binary FDistribution

fDistribution :: Int -> Int -> FDistribution
fDistribution n m
  | n > 0 && m > 0 =
    let n' = fromIntegral n
        m' = fromIntegral m
        f' = 0.5 * (log m' * m' + log n' * n') - logBeta (0.5*n') (0.5*m')
    in F n' m' f'
  | otherwise =
    error "Statistics.Distribution.FDistribution.fDistribution: non-positive number of degrees of freedom"

instance D.Distribution FDistribution where
  cumulative = cumulative

instance D.ContDistr FDistribution where
  density  = density
  quantile = quantile

cumulative :: FDistribution -> Double -> Double
cumulative (F n m _) x
  | x <= 0       = 0
  | isInfinite x = 1            -- Only matches +∞
  | x > 0        = let y = n*x in incompleteBeta (0.5 * n) (0.5 * m) (y / (m + y))

density :: FDistribution -> Double -> Double
density (F n m fac) x
  | x > 0     = exp $ fac + log x * (0.5 * n - 1) - log(m + n*x) * 0.5 * (n + m)
  | otherwise = 0

quantile :: FDistribution -> Double -> Double
quantile (F n m _) p
  | p >= 0 && p <= 1 =
    let x = invIncompleteBeta (0.5 * n) (0.5 * m) p
    in m * x / (n * (1 - x))
  | otherwise =
    error $ "Statistics.Distribution.Uniform.quantile: p must be in [0,1] range. Got: "++show p


instance D.MaybeMean FDistribution where
  maybeMean (F _ m _) | m > 2     = Just $ m / (m - 2)
                      | otherwise = Nothing

instance D.MaybeVariance FDistribution where
  maybeStdDev (F n m _)
    | m > 4     = Just $ 2 * sqr m * (m + n - 2) / (n * sqr (m - 2) * (m - 4))
    | otherwise = Nothing

instance D.ContGen FDistribution where
  genContVar = D.genContinous

sqr :: Double -> Double
sqr x = x * x
{-# INLINE sqr #-}
