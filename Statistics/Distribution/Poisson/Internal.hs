-- |
-- Module    : Statistics.Distribution.Poisson.Internal
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Internal code for the Poisson distribution.

module Statistics.Distribution.Poisson.Internal
    (
      probability
    ) where

import Numeric.MathFunctions.Constants (m_sqrt_2_pi, m_tiny)
import Numeric.SpecFunctions           (logGamma, stirlingError)
import Numeric.SpecFunctions.Extra     (bd0)

-- | An unchecked, non-integer-valued version of Loader's saddle point
-- algorithm.
probability :: Double -> Double -> Double
probability 0      0     = 1
probability 0      1     = 0
probability lambda x
  | isInfinite lambda    = 0
  | x < 0                = 0
  | x <= lambda * m_tiny = exp (-lambda)
  | lambda < x * m_tiny  = exp (-lambda + x * log lambda - logGamma (x+1))
  | otherwise            = exp (-(stirlingError x) - bd0 x lambda) /
                           (m_sqrt_2_pi * sqrt x)
{-# INLINE probability #-}
