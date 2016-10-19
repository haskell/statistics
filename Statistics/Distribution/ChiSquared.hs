{-# LANGUAGE OverloadedStrings #-}
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
        , chiSquaredNDF
        -- * Constructors
        , chiSquared
        , chiSquaredE
        ) where

import Control.Applicative
import Data.Aeson            (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary           (Binary(..))
import Data.Data             (Data, Typeable)
import GHC.Generics          (Generic)
import Numeric.SpecFunctions ( incompleteGamma,invIncompleteGamma,logGamma,digamma)
import Numeric.MathFunctions.Constants (m_neg_inf)
import qualified System.Random.MWC.Distributions as MWC

import qualified Statistics.Distribution         as D
import Statistics.Internal



-- | Chi-squared distribution
newtype ChiSquared = ChiSquared
  { chiSquaredNDF :: Int
    -- ^ Get number of degrees of freedom
  }
  deriving (Eq, Typeable, Data, Generic)

instance Show ChiSquared where
  showsPrec i (ChiSquared n) = defaultShow1 "chiSquared" n i
instance Read ChiSquared where
  readPrec = defaultReadPrecM1 "chiSquared" chiSquaredE

instance ToJSON ChiSquared
instance FromJSON ChiSquared where
  parseJSON (Object v) = do
    n <- v .: "chiSquaredNDF"
    maybe (fail $ errMsg n) return $ chiSquaredE n
  parseJSON _ = empty

instance Binary ChiSquared where
  put (ChiSquared x) = put x
  get = do n <- get
           maybe (fail $ errMsg n) return $ chiSquaredE n


-- | Construct chi-squared distribution. Number of degrees of freedom
--   must be positive.
chiSquared :: Int -> ChiSquared
chiSquared n = maybe (error $ errMsg n) id $ chiSquaredE n

-- | Construct chi-squared distribution. Number of degrees of freedom
--   must be positive.
chiSquaredE :: Int -> Maybe ChiSquared
chiSquaredE n
  | n <= 0    = Nothing
  | otherwise = Just (ChiSquared n)

errMsg :: Int -> String
errMsg n = "Statistics.Distribution.ChiSquared.chiSquared: N.D.F. must be positive. Got " ++ show n

instance D.Distribution ChiSquared where
  cumulative = cumulative

instance D.ContDistr ChiSquared where
  density chi x
    | x <= 0    = 0
    | otherwise = exp $ log x * (ndf2 - 1) - x2 - logGamma ndf2 - log 2 * ndf2
    where
      ndf  = fromIntegral $ chiSquaredNDF chi
      ndf2 = ndf/2
      x2   = x/2

  logDensity chi x
    | x <= 0    = m_neg_inf
    | otherwise = log x * (ndf2 - 1) - x2 - logGamma ndf2 - log 2 * ndf2
    where
      ndf  = fromIntegral $ chiSquaredNDF chi
      ndf2 = ndf/2
      x2   = x/2

  quantile = quantile

instance D.Mean ChiSquared where
    mean (ChiSquared ndf) = fromIntegral ndf

instance D.Variance ChiSquared where
    variance (ChiSquared ndf) = fromIntegral (2*ndf)

instance D.MaybeMean ChiSquared where
    maybeMean = Just . D.mean

instance D.MaybeVariance ChiSquared where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy ChiSquared where
  entropy (ChiSquared ndf) =
    let kHalf = 0.5 * fromIntegral ndf in
    kHalf
    + log 2
    + logGamma kHalf
    + (1-kHalf) * digamma kHalf

instance D.MaybeEntropy ChiSquared where
  maybeEntropy = Just . D.entropy

instance D.ContGen ChiSquared where
    genContVar (ChiSquared n) = MWC.chiSquare n


cumulative :: ChiSquared -> Double -> Double
cumulative chi x
  | x <= 0    = 0
  | otherwise = incompleteGamma (ndf/2) (x/2)
  where
    ndf = fromIntegral $ chiSquaredNDF chi

quantile :: ChiSquared -> Double -> Double
quantile (ChiSquared ndf) p
  | p == 0         = 0
  | p == 1         = 1/0
  | p > 0 && p < 1 = 2 * invIncompleteGamma (fromIntegral ndf / 2) p
  | otherwise      =
    error $ "Statistics.Distribution.ChiSquared.quantile: p must be in [0,1] range. Got: "++show p
