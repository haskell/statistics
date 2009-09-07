-- |
-- Module    : Statistics.Internal
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Types and functions common to much statistics code.

module Statistics.Internal
    (
      huge
    , sqrt_2
    , sqrt_2_pi
    ) where

huge :: Double
huge = 1e308
{-# INLINE huge #-}

sqrt_2 :: Double
sqrt_2 = sqrt 2
{-# INLINE sqrt_2 #-}

sqrt_2_pi :: Double
sqrt_2_pi = sqrt (2 * pi)
{-# INLINE sqrt_2_pi #-}
