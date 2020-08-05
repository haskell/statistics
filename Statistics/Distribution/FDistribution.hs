{-# LANGUAGE OverloadedStrings #-}
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
    -- * Constructors
  , fDistribution
  , fDistributionE
  , fDistributionReal
  , fDistributionRealE
    -- * Accessors
  , fDistributionNDF1
  , fDistributionNDF2
  ) where

import Control.Applicative
import Data.Aeson             (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary            (Binary(..))
import Data.Data              (Data, Typeable)
import GHC.Generics           (Generic)
import Numeric.SpecFunctions (
  logBeta, incompleteBeta, invIncompleteBeta, digamma)
import Numeric.MathFunctions.Constants (m_neg_inf)

import qualified Statistics.Distribution as D
import Statistics.Function (square)
import Statistics.Internal


-- | F distribution
data FDistribution = F { fDistributionNDF1 :: {-# UNPACK #-} !Double
                       , fDistributionNDF2 :: {-# UNPACK #-} !Double
                       , _pdfFactor        :: {-# UNPACK #-} !Double
                       }
                   deriving (Eq, Typeable, Data, Generic)

instance Show FDistribution where
  showsPrec i (F n m _) = defaultShow2 "fDistributionReal" n m i
instance Read FDistribution where
  readPrec = defaultReadPrecM2 "fDistributionReal" fDistributionRealE

instance ToJSON FDistribution
instance FromJSON FDistribution where
  parseJSON (Object v) = do
    n <- v .: "fDistributionNDF1"
    m <- v .: "fDistributionNDF2"
    maybe (fail $ errMsgR n m) return $ fDistributionRealE n m
  parseJSON _ = empty

instance Binary FDistribution where
  put (F n m _) = put n >> put m
  get = do
    n <- get
    m <- get
    maybe (fail $ errMsgR n m) return $ fDistributionRealE n m

fDistribution :: Int -> Int -> FDistribution
fDistribution n m = maybe (error $ errMsg n m) id $ fDistributionE n m

fDistributionReal :: Double -> Double -> FDistribution
fDistributionReal n m = maybe (error $ errMsgR n m) id $ fDistributionRealE n m

fDistributionE :: Int -> Int -> Maybe FDistribution
fDistributionE n m
  | n > 0 && m > 0 =
    let n' = fromIntegral n
        m' = fromIntegral m
        f' = 0.5 * (log m' * m' + log n' * n') - logBeta (0.5*n') (0.5*m')
    in Just $ F n' m' f'
  | otherwise = Nothing

fDistributionRealE :: Double -> Double -> Maybe FDistribution
fDistributionRealE n m
  | n > 0 && m > 0 =
    let f' = 0.5 * (log m * m + log n * n) - logBeta (0.5*n) (0.5*m)
    in Just $ F n m f'
  | otherwise = Nothing

errMsg :: Int -> Int -> String
errMsg _ _ = "Statistics.Distribution.FDistribution.fDistribution: non-positive number of degrees of freedom"

errMsgR :: Double -> Double -> String
errMsgR _ _ = "Statistics.Distribution.FDistribution.fDistribution: non-positive number of degrees of freedom"



instance D.Distribution FDistribution where
  cumulative      = cumulative
  complCumulative = complCumulative

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
  -- Only matches +∞
  | isInfinite x = 1
  -- NOTE: Here we rely on implementation detail of incompleteBeta. It
  --       computes using series expansion for sufficiently small x
  --       and uses following identity otherwise:
  --
  --           I(x; a, b) = 1 - I(1-x; b, a)
  --
  --       Point is we can compute 1-x as m/(m+y) without loss of
  --       precision for large x. Sadly this switchover point is
  --       implementation detail.
  | n >= (n+m)*bx = incompleteBeta (0.5 * n) (0.5 * m) bx
  | otherwise     = 1 - incompleteBeta (0.5 * m) (0.5 * n) bx1
  where
    y   = n * x
    bx  = y / (m + y)
    bx1 = m / (m + y)

complCumulative :: FDistribution -> Double -> Double
complCumulative (F n m _) x
  | x <= 0        = 1
  -- Only matches +∞
  | isInfinite x  = 0
  -- See NOTE at cumulative
  | m >= (n+m)*bx = incompleteBeta (0.5 * m) (0.5 * n) bx
  | otherwise     = 1 - incompleteBeta (0.5 * n) (0.5 * m) bx1
  where
    y   = n*x
    bx  = m / (m + y)
    bx1 = y / (m + y)

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
  genContVar = D.genContinuous
