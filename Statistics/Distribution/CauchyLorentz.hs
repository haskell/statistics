{-# LANGUAGE DeriveDataTypeable #-}
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
  , cauchyDistribScale
    -- * Constructors
  , cauchyDistribution
  , standardCauchy
  ) where

import Data.Typeable (Typeable)
import Statistics.Constants (m_sqrt_2, m_sqrt_2_pi)
import qualified Statistics.Distribution as D


-- | Cauchy-Lorentz distribution.
data CauchyDistribution = CD {
    cauchyMean          :: {-# UNPACK #-} !Double
    -- | Scale parameter of Cauchy-Lorentz distribution. It's
    --   different from variance and specify half width at half
    --   maximum (HWHM).
  , cauchyDistribScale  :: {-# UNPACK #-} !Double
  }
  deriving (Eq,Show,Read,Typeable)

-- | Cauchy distribution
cauchyDistribution :: Double    -- ^ Central point
                   -> Double    -- ^ Scale parameter (FWHM)
                   -> CauchyDistribution
cauchyDistribution m s
  | s > 0     = CD m s
  | otherwise =
    error $ "Statistics.Distribution.CauchyLorentz.cauchyDistribution: FWHM must be positive. Got " ++ show s

standardCauchy :: CauchyDistribution
standardCauchy = CD 0 1


instance D.Distribution CauchyDistribution where
  cumulative (CD m s) x = 0.5 + atan( (x - m) / s ) / pi

instance D.ContDistr CauchyDistribution where
  density (CD m s) x = (1 / pi) / (s * (1 + y*y))
    where y = (x - m) / s
  quantile (CD m s) p
    | p > 0 && p < 1 = m + s * tan( pi * (p - 0.5) )
    | p == 0         = -1 / 0
    | p == 1         =  1 / 0
    | otherwise      =
      error $ "Statistics.Distribution.CauchyLorentz..quantile: p must be in [0,1] range. Got: "++show p
