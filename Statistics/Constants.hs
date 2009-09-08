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
m_sqrt_2 = 1.414213562373095145474621858739
{-# INLINE m_sqrt_2 #-}

-- | @sqrt (2 * pi)@
m_sqrt_2_pi :: Double
m_sqrt_2_pi = 2.506628274631000241612355239340
{-# INLINE m_sqrt_2_pi #-}

-- | @2 / sqrt pi@
m_2_sqrt_pi :: Double
m_2_sqrt_pi = 1.128379167095512558560699289956
{-# INLINE m_2_sqrt_pi #-}

-- | @1 / sqrt 2@
m_1_sqrt_2 :: Double
m_1_sqrt_2 = 0.707106781186547461715008466854
{-# INLINE m_1_sqrt_2 #-}
