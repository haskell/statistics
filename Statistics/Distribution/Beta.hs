{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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
  ( BetaDistribution
    -- * Constructor
  , betaDistr
  , improperBetaDistr
    -- * Accessors
  , bdAlpha
  , bdBeta
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Numeric.SpecFunctions (
  incompleteBeta, invIncompleteBeta, logBeta, digamma)
import Numeric.MathFunctions.Constants (m_NaN)
import qualified Statistics.Distribution as D
import Data.Binary (put, get)
import Control.Applicative ((<$>), (<*>))

-- | The beta distribution
data BetaDistribution = BD
 { bdAlpha :: {-# UNPACK #-} !Double
   -- ^ Alpha shape parameter
 , bdBeta  :: {-# UNPACK #-} !Double
   -- ^ Beta shape parameter
 } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON BetaDistribution
instance ToJSON BetaDistribution

instance Binary BetaDistribution where
    put (BD x y) = put x >> put y
    get = BD <$> get <*> get

-- | Create beta distribution. Both shape parameters must be positive.
betaDistr :: Double             -- ^ Shape parameter alpha
          -> Double             -- ^ Shape parameter beta
          -> BetaDistribution
betaDistr a b
  | a > 0 && b > 0 = improperBetaDistr a b
  | otherwise      =
      error $  "Statistics.Distribution.Beta.betaDistr: "
            ++ "shape parameters must be positive. Got a = "
            ++ show a
            ++ " b = "
            ++ show b

-- | Create beta distribution. This construtor doesn't check parameters.
improperBetaDistr :: Double             -- ^ Shape parameter alpha
                  -> Double             -- ^ Shape parameter beta
                  -> BetaDistribution
improperBetaDistr = BD

instance D.Distribution BetaDistribution where
  cumulative (BD a b) x
    | x <= 0    = 0
    | x >= 1    = 1
    | otherwise = incompleteBeta a b x

instance D.Mean BetaDistribution where
  mean (BD a b) = a / (a + b)

instance D.MaybeMean BetaDistribution where
  maybeMean = Just . D.mean

instance D.Variance BetaDistribution where
  variance (BD a b) = a*b / (apb*apb*(apb+1))
    where apb = a + b

instance D.MaybeVariance BetaDistribution where
  maybeVariance = Just . D.variance

instance D.Entropy BetaDistribution where
  entropy (BD a b) =
    logBeta a b
    - (a-1) * digamma a
    - (b-1) * digamma b
    + (a+b-2) * digamma (a+b)

instance D.MaybeEntropy BetaDistribution where
  maybeEntropy = Just . D.entropy

instance D.ContDistr BetaDistribution where
  density (BD a b) x
   | a <= 0 || b <= 0 = m_NaN
   | x <= 0 = 0
   | x >= 1 = 0
   | otherwise = exp $ (a-1)*log x + (b-1)*log (1-x) - logBeta a b

  quantile (BD a b) p
    | p == 0         = 0
    | p == 1         = 1
    | p > 0 && p < 1 = invIncompleteBeta a b p
    | otherwise      =
        error $ "Statistics.Distribution.Gamma.quantile: p must be in [0,1] range. Got: "++show p

instance D.ContGen BetaDistribution where
  genContVar = D.genContinuous
