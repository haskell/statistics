-- |
-- Module    : Statistics.Constants
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Constant values common to much statistics code.

module Statistics.Constants
    (
      m_huge
    , m_1_sqrt_2
    , m_2_sqrt_pi
    , m_sqrt_2
    , m_sqrt_2_pi
    ) where

-- | A very large number.
m_huge :: Double
m_huge = 1.797693e308
{-# INLINE m_huge #-}

-- | @sqrt 2@
m_sqrt_2 :: Double
m_sqrt_2 = 1.4142135623730950488016887242096980785696718753769480731766
{-# INLINE m_sqrt_2 #-}

-- | @sqrt (2 * pi)@
m_sqrt_2_pi :: Double
m_sqrt_2_pi = 2.5066282746310005024157652848110452530069867406099383166299
{-# INLINE m_sqrt_2_pi #-}

-- | @2 / sqrt pi@
m_2_sqrt_pi :: Double
m_2_sqrt_pi = 1.1283791670955125738961589031215451716881012586579977136881
{-# INLINE m_2_sqrt_pi #-}

-- | @1 / sqrt 2@
m_1_sqrt_2 :: Double
m_1_sqrt_2 = 0.7071067811865475244008443621048490392848359376884740365883
{-# INLINE m_1_sqrt_2 #-}
