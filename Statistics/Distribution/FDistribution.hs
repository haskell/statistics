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

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Numeric.MathFunctions.Constants (m_neg_inf)
import GHC.Generics (Generic)
import qualified Statistics.Distribution as D
import Statistics.Function (square)
import Numeric.SpecFunctions (
  logBeta, incompleteBeta, invIncompleteBeta, digamma)
import Data.Binary (put, get)
import Control.Applicative ((<$>), (<*>))

-- | F distribution
data FDistribution = F { fDistributionNDF1 :: {-# UNPACK #-} !Double
                       , fDistributionNDF2 :: {-# UNPACK #-} !Double
                       , _pdfFactor        :: {-# UNPACK #-} !Double
                       }
                   deriving (Eq, Show, Read, Typeable, Data, Generic)

instance FromJSON FDistribution
instance ToJSON FDistribution

instance Binary FDistribution where
    get = F <$> get <*> get <*> get
    put (F x y z) = put x >> put y >> put z

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
  density d x
    | x <= 0    = 0
    | otherwise = exp $ logDensity d x
  logDensity d x
    | x <= 0    = m_neg_inf
    | otherwise = logDensity d x
  quantile = quantile

cumulative :: FDistribution -> Double -> Double
cumulative (F n m _) x
  | x <= 0       = 0
  | isInfinite x = 1            -- Only matches +∞
  | otherwise    = let y = n*x in incompleteBeta (0.5 * n) (0.5 * m) (y / (m + y))

logDensity :: FDistribution -> Double -> Double
logDensity (F n m fac) x
  = fac + log x * (0.5 * n - 1) - log(m + n*x) * 0.5 * (n + m)

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
    | m > 4     = Just $ 2 * square m * (m + n - 2) / (n * square (m - 2) * (m - 4))
    | otherwise = Nothing

instance D.Entropy FDistribution where
  entropy (F n m _) =
    let nHalf = 0.5 * n
        mHalf = 0.5 * m in
    log (n/m)
    + logBeta nHalf mHalf
    + (1 - nHalf) * digamma nHalf
    - (1 + mHalf) * digamma mHalf
    + (nHalf + mHalf) * digamma (nHalf + mHalf)

instance D.MaybeEntropy FDistribution where
  maybeEntropy = Just . D.entropy

instance D.ContGen FDistribution where
  genContVar = D.genContinous
