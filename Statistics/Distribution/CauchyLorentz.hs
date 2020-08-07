{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.CauchyLorentz
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Cauchy-Lorentz distribution. It's also known as Lorentz
-- distribution or Breit–Wigner distribution.
--
-- It doesn't have mean and variance.
module Statistics.Distribution.CauchyLorentz (
    CauchyDistribution
  , cauchyDistribMedian
  , cauchyDistribScale
    -- * Constructors
  , cauchyDistribution
  , cauchyDistributionE
  , standardCauchy
  ) where

import Control.Applicative
import Data.Aeson             (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary            (Binary(..))
import Data.Maybe             (fromMaybe)
import Data.Data              (Data, Typeable)
import GHC.Generics           (Generic)
import qualified Statistics.Distribution as D
import Statistics.Internal

-- | Cauchy-Lorentz distribution.
data CauchyDistribution = CD {
    -- | Central value of Cauchy-Lorentz distribution which is its
    --   mode and median. Distribution doesn't have mean so function
    --   is named after median.
    cauchyDistribMedian :: {-# UNPACK #-} !Double
    -- | Scale parameter of Cauchy-Lorentz distribution. It's
    --   different from variance and specify half width at half
    --   maximum (HWHM).
  , cauchyDistribScale  :: {-# UNPACK #-} !Double
  }
  deriving (Eq, Typeable, Data, Generic)

instance Show CauchyDistribution where
  showsPrec i (CD m s) = defaultShow2 "cauchyDistribution" m s i
instance Read CauchyDistribution where
  readPrec = defaultReadPrecM2 "cauchyDistribution" cauchyDistributionE

instance ToJSON   CauchyDistribution
instance FromJSON CauchyDistribution where
  parseJSON (Object v) = do
    m <- v .: "cauchyDistribMedian"
    s <- v .: "cauchyDistribScale"
    maybe (fail $ errMsg m s) return $ cauchyDistributionE m s
  parseJSON _ = empty

instance Binary CauchyDistribution where
    put (CD m s) = put m >> put s
    get = do
      m <- get
      s <- get
      maybe (error $ errMsg m s) return $ cauchyDistributionE m s


-- | Cauchy distribution
cauchyDistribution :: Double    -- ^ Central point
                   -> Double    -- ^ Scale parameter (FWHM)
                   -> CauchyDistribution
cauchyDistribution m s
  = fromMaybe (error $ errMsg m s)
  $ cauchyDistributionE m s


-- | Cauchy distribution
cauchyDistributionE :: Double    -- ^ Central point
                    -> Double    -- ^ Scale parameter (FWHM)
                    -> Maybe CauchyDistribution
cauchyDistributionE m s
  | s > 0     = Just (CD m s)
  | otherwise = Nothing

errMsg :: Double -> Double -> String
errMsg _ s
  = "Statistics.Distribution.CauchyLorentz.cauchyDistribution: FWHM must be positive. Got "
  ++ show s

-- | Standard Cauchy distribution. It's centered at 0 and have 1 FWHM
standardCauchy :: CauchyDistribution
standardCauchy = CD 0 1


instance D.Distribution CauchyDistribution where
  cumulative (CD m s) x
    | y < -1    = atan (-1/y) / pi
    | otherwise = 0.5 + atan y / pi
    where
       y = (x - m) / s
  complCumulative (CD m s) x
    | y > 1     = atan (1/y) / pi
    | otherwise = 0.5 - atan y / pi
    where
       y = (x - m) / s

instance D.ContDistr CauchyDistribution where
  density (CD m s) x = (1 / pi) / (s * (1 + y*y))
    where y = (x - m) / s
  quantile (CD m s) p
    | p == 0    = -1 / 0
    | p == 1    =  1 / 0
    | p == 0.5  = m
    | p < 0     = err
    | p < 0.5   = m - s / tan( pi * p )
    | p < 1     = m + s / tan( pi * (1 - p) )
    | otherwise = err
    where
      err = error
          $ "Statistics.Distribution.CauchyLorentz.quantile: p must be in [0,1] range. Got: "++show p
  complQuantile (CD m s) p
    | p == 0    =  1 / 0
    | p == 1    = -1 / 0
    | p == 0.5  = m
    | p < 0     = err
    | p < 0.5   = m + s / tan( pi * p )
    | p < 1     = m - s / tan( pi * (1 - p) )
    | otherwise = err
    where
      err = error
          $ "Statistics.Distribution.CauchyLorentz.quantile: p must be in [0,1] range. Got: "++show p


instance D.ContGen CauchyDistribution where
  genContVar = D.genContinuous

instance D.Entropy CauchyDistribution where
  entropy (CD _ s) = log s + log (4*pi)

instance D.MaybeEntropy CauchyDistribution where
  maybeEntropy = Just . D.entropy
