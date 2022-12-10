{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Laplace
-- Copyright : (c) 2015 Mihai Maruseac
-- License   : BSD3
--
-- Maintainer  : mihai.maruseac@maruseac.com
-- Stability   : experimental
-- Portability : portable
--
-- The Laplace distribution.  This is the continuous probability
-- defined as the difference of two iid exponential random variables
-- or a Brownian motion evaluated as exponentially distributed times.
-- It is used in differential privacy (Laplace Method), speech
-- recognition and least absolute deviations method (Laplace's first
-- law of errors, giving a robust regression method)
--
module Statistics.Distribution.Laplace
    (
      LaplaceDistribution
    -- * Constructors
    , laplace
    , laplaceE
    -- * Accessors
    , ldLocation
    , ldScale
    ) where

import Control.Applicative
import Data.Aeson           (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary          (Binary(..))
import Data.Data            (Data, Typeable)
import GHC.Generics         (Generic)
import qualified Data.Vector.Generic             as G
import qualified Statistics.Distribution         as D
import qualified Statistics.Quantile             as Q
import qualified Statistics.Sample               as S
import Statistics.Internal


data LaplaceDistribution = LD {
      ldLocation :: {-# UNPACK #-} !Double
    -- ^ Location.
    , ldScale    :: {-# UNPACK #-} !Double
    -- ^ Scale.
    } deriving (Eq, Typeable, Data, Generic)

instance Show LaplaceDistribution where
  showsPrec i (LD l s) = defaultShow2 "laplace" l s i
instance Read LaplaceDistribution where
  readPrec = defaultReadPrecM2 "laplace" laplaceE

instance ToJSON LaplaceDistribution
instance FromJSON LaplaceDistribution where
  parseJSON (Object v) = do
    l <- v .: "ldLocation"
    s <- v .: "ldScale"
    maybe (fail $ errMsg l s) return $ laplaceE l s
  parseJSON _ = empty

instance Binary LaplaceDistribution where
  put (LD l s) = put l >> put s
  get = do
    l <- get
    s <- get
    maybe (fail $ errMsg l s) return $ laplaceE l s

instance D.Distribution LaplaceDistribution where
    cumulative      = cumulative
    complCumulative = complCumulative

instance D.ContDistr LaplaceDistribution where
    density    (LD l s) x = exp (- abs (x - l) / s) / (2 * s)
    logDensity (LD l s) x = - abs (x - l) / s - log 2 - log s
    quantile      = quantile
    complQuantile = complQuantile

instance D.Mean LaplaceDistribution where
    mean (LD l _) = l

instance D.Variance LaplaceDistribution where
    variance (LD _ s) = 2 * s * s

instance D.MaybeMean LaplaceDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance LaplaceDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy LaplaceDistribution where
  entropy (LD _ s) = 1 + log (2 * s)

instance D.MaybeEntropy LaplaceDistribution where
  maybeEntropy = Just . D.entropy

instance D.ContGen LaplaceDistribution where
  genContVar = D.genContinuous

cumulative :: LaplaceDistribution -> Double -> Double
cumulative (LD l s) x
  | x <= l    = 0.5 * exp ( (x - l) / s)
  | otherwise = 1 - 0.5 * exp ( - (x - l) / s )

complCumulative :: LaplaceDistribution -> Double -> Double
complCumulative (LD l s) x
  | x <= l    = 1 - 0.5 * exp ( (x - l) / s)
  | otherwise = 0.5 * exp ( - (x - l) / s )

quantile :: LaplaceDistribution -> Double -> Double
quantile (LD l s) p
  | p == 0             = -inf
  | p == 1             = inf
  | p == 0.5           = l
  | p > 0   && p < 0.5 = l + s * log (2 * p)
  | p > 0.5 && p < 1   = l - s * log (2 - 2 * p)
  | otherwise          =
    error $ "Statistics.Distribution.Laplace.quantile: p must be in [0,1] range. Got: "++show p
  where
    inf = 1 / 0

complQuantile :: LaplaceDistribution -> Double -> Double
complQuantile (LD l s) p
  | p == 0             = inf
  | p == 1             = -inf
  | p == 0.5           = l
  | p > 0   && p < 0.5 = l - s * log (2 * p)
  | p > 0.5 && p < 1   = l + s * log (2 - 2 * p)
  | otherwise          =
    error $ "Statistics.Distribution.Laplace.quantile: p must be in [0,1] range. Got: "++show p
  where
    inf = 1 / 0

-- | Create an Laplace distribution.
laplace :: Double         -- ^ Location
        -> Double        -- ^ Scale
        -> LaplaceDistribution
laplace l s = maybe (error $ errMsg l s) id $ laplaceE l s

-- | Create an Laplace distribution.
laplaceE :: Double         -- ^ Location
         -> Double        -- ^ Scale
         -> Maybe LaplaceDistribution
laplaceE l s
  | s >= 0    = Just (LD l s)
  | otherwise = Nothing

errMsg :: Double -> Double -> String
errMsg _ s = "Statistics.Distribution.Laplace.laplace: scale parameter must be positive. Got " ++ show s


-- | Create Laplace distribution from sample.  The location is estimated
--   as the median of the sample, and the scale as the mean absolute
--   deviation of the median.
instance D.FromSample LaplaceDistribution Double where
  fromSample xs
    | G.null xs = Nothing
    | otherwise = Just $! LD s l
    where
      s = Q.median Q.medianUnbiased xs
      l = S.mean $ G.map (\x -> abs $ x - s) xs
