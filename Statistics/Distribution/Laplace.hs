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
    , laplaceFromSample
    -- * Accessors
    , ldLocation
    , ldScale
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Data.Vector.Generic             as G
import qualified Statistics.Distribution         as D
import qualified Statistics.Quantile             as Q
import qualified Statistics.Sample               as S
import Statistics.Types (Sample)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (put, get)


data LaplaceDistribution = LD {
      ldLocation :: {-# UNPACK #-} !Double
    -- ^ Location.
    , ldScale    :: {-# UNPACK #-} !Double
    -- ^ Scale.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON LaplaceDistribution
instance ToJSON LaplaceDistribution

instance Binary LaplaceDistribution where
    put (LD l s) = put l >> put s
    get = LD <$> get <*> get

instance D.Distribution LaplaceDistribution where
    cumulative      = cumulative
    complCumulative = complCumulative

instance D.ContDistr LaplaceDistribution where
    density    (LD l s) x = exp (- abs (x - l) / s) / (2 * s)
    logDensity (LD l s) x = - abs (x - l) / s - log 2 - log s
    quantile = quantile

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
  genContVar = D.genContinous

cumulative :: LaplaceDistribution -> Double -> Double
cumulative (LD l s) x = let d = x - l in 0.5 + 0.5 * signum d * (1 - exp (- abs d / s))

complCumulative :: LaplaceDistribution -> Double -> Double
complCumulative (LD l s) x = let d = x - l in 0.5 - 0.5 * signum d * (1 - exp (- abs d / s))

quantile :: LaplaceDistribution -> Double -> Double
quantile (LD l s) p
  | p == 0         = -inf
  | p == 1         = inf
  | p == 0.5       = l
  | p > 0 && p < 1 = let d = p - 0.5 in l - s * signum d * log (1 - 2 * abs d)
  | otherwise      =
    error $ "Statistics.Distribution.Laplace.quantile: p must be in [0,1] range. Got: "++show p
  where inf        = 1 / 0

-- | Create an laplace distribution.
laplace :: Double            -- ^ Location
            -> Double        -- ^ Scale
            -> LaplaceDistribution
laplace l s
  | s <= 0 =
    error $ "Statistics.Distribution.Laplace.laplace: scale parameter must be positive. Got " ++ show s
  | otherwise = LD l s

-- | Create Laplace distribution from sample. No tests are made to
-- check whether it truly is Laplace.
laplaceFromSample :: Sample -> LaplaceDistribution
laplaceFromSample xs = LD s l
  where
    s = Q.continuousBy Q.medianUnbiased 1 2 xs
    l = S.mean $ G.map (\x -> abs $ x - s) xs
