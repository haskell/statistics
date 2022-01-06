{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Gamma
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The gamma distribution.  This is a continuous probability
-- distribution with two parameters, /k/ and &#977;. If /k/ is
-- integral, the distribution represents the sum of /k/ independent
-- exponentially distributed random variables, each of which has a
-- mean of &#977;.

module Statistics.Distribution.Gamma
    (
      GammaDistribution
    -- * Constructors
    , gammaDistr
    , gammaDistrE
    , improperGammaDistr
    , improperGammaDistrE
    -- * Accessors
    , gdShape
    , gdScale
    ) where

import Control.Applicative
import Data.Aeson           (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary          (Binary(..))
import Data.Data            (Data, Typeable)
import GHC.Generics         (Generic)
import Numeric.MathFunctions.Constants (m_pos_inf, m_NaN, m_neg_inf)
import Numeric.SpecFunctions (incompleteGamma, invIncompleteGamma, logGamma, digamma)
import qualified System.Random.MWC.Distributions as MWC
import qualified Numeric.Sum as Sum

import Statistics.Distribution.Poisson.Internal as Poisson
import qualified Statistics.Distribution as D
import Statistics.Internal


-- | The gamma distribution.
data GammaDistribution = GD {
      gdShape :: {-# UNPACK #-} !Double -- ^ Shape parameter, /k/.
    , gdScale :: {-# UNPACK #-} !Double -- ^ Scale parameter, &#977;.
    } deriving (Eq, Typeable, Data, Generic)

instance Show GammaDistribution where
  showsPrec i (GD k theta) = defaultShow2 "improperGammaDistr" k theta i
instance Read GammaDistribution where
  readPrec = defaultReadPrecM2 "improperGammaDistr" improperGammaDistrE


instance ToJSON GammaDistribution
instance FromJSON GammaDistribution where
  parseJSON (Object v) = do
    k     <- v .: "gdShape"
    theta <- v .: "gdScale"
    maybe (fail $ errMsgI k theta) return $ improperGammaDistrE k theta
  parseJSON _ = empty

instance Binary GammaDistribution where
  put (GD x y) = put x >> put y
  get = do
    k     <- get
    theta <- get
    maybe (fail $ errMsgI k theta) return $ improperGammaDistrE k theta


-- | Create gamma distribution. Both shape and scale parameters must
-- be positive.
gammaDistr :: Double            -- ^ Shape parameter. /k/
           -> Double            -- ^ Scale parameter, &#977;.
           -> GammaDistribution
gammaDistr k theta
  = maybe (error $ errMsg k theta) id $ gammaDistrE k theta

errMsg :: Double -> Double -> String
errMsg k theta
  =  "Statistics.Distribution.Gamma.gammaDistr: "
  ++ "k=" ++ show k
  ++ "theta=" ++ show theta
  ++ " but must be positive"

-- | Create gamma distribution. Both shape and scale parameters must
-- be positive.
gammaDistrE :: Double            -- ^ Shape parameter. /k/
            -> Double            -- ^ Scale parameter, &#977;.
            -> Maybe GammaDistribution
gammaDistrE k theta
  | k > 0 && theta > 0 = Just (GD k theta)
  | otherwise          = Nothing


-- | Create gamma distribution. Both shape and scale parameters must
-- be non-negative.
improperGammaDistr :: Double            -- ^ Shape parameter. /k/
                   -> Double            -- ^ Scale parameter, &#977;.
                   -> GammaDistribution
improperGammaDistr k theta
  = maybe (error $ errMsgI k theta) id $ improperGammaDistrE k theta

errMsgI :: Double -> Double -> String
errMsgI k theta
  =  "Statistics.Distribution.Gamma.gammaDistr: "
  ++ "k=" ++ show k
  ++ "theta=" ++ show theta
  ++ " but must be non-negative"

-- | Create gamma distribution. Both shape and scale parameters must
-- be non-negative.
improperGammaDistrE :: Double            -- ^ Shape parameter. /k/
                    -> Double            -- ^ Scale parameter, &#977;.
                    -> Maybe GammaDistribution
improperGammaDistrE k theta
  | k >= 0 && theta >= 0 = Just (GD k theta)
  | otherwise            = Nothing

instance D.Distribution GammaDistribution where
    cumulative = cumulative

instance D.ContDistr GammaDistribution where
    density    = density
    logDensity (GD k theta) x
      | x <= 0    = m_neg_inf
      | otherwise = Sum.sum Sum.kbn [ log x * (k - 1)
                                    , - (x / theta)
                                    , - logGamma k
                                    , - log theta * k
                                    ]
    quantile   = quantile

instance D.Variance GammaDistribution where
    variance (GD a l) = a * l * l

instance D.Mean GammaDistribution where
    mean (GD a l) = a * l

instance D.MaybeMean GammaDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance GammaDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.MaybeEntropy GammaDistribution where
  maybeEntropy (GD a l)
    | a > 0 && l > 0 =
      Just $
      a
      + log l
      + logGamma a
      + (1-a) * digamma a
    | otherwise = Nothing

instance D.ContGen GammaDistribution where
    genContVar (GD a l) = MWC.gamma a l


density :: GammaDistribution -> Double -> Double
density (GD a l) x
  | a < 0 || l <= 0   = m_NaN
  | x <= 0            = 0
  | a == 0            = if x == 0 then m_pos_inf else 0
  | x == 0            = if a < 1 then m_pos_inf else if a > 1 then 0 else 1/l
  | a < 1             = Poisson.probability (x/l) a * a / x
  | otherwise         = Poisson.probability (x/l) (a-1) / l

cumulative :: GammaDistribution -> Double -> Double
cumulative (GD k l) x
  | x <= 0    = 0
  | otherwise = incompleteGamma k (x/l)

quantile :: GammaDistribution -> Double -> Double
quantile (GD k l) p
  | p == 0         = 0
  | p == 1         = 1/0
  | p > 0 && p < 1 = l * invIncompleteGamma k p
  | otherwise      =
    error $ "Statistics.Distribution.Gamma.quantile: p must be in [0,1] range. Got: "++show p
