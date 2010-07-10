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
      m_epsilon
    , m_huge
    , m_1_sqrt_2
    , m_2_sqrt_pi
    , m_ln_sqrt_2_pi
    , m_max_exp
    , m_sqrt_2
    , m_sqrt_2_pi
    , m_pos_inf
    , m_neg_inf
    , m_NaN
    ) where

-- | A very large number.
m_huge :: Double
m_huge = 1.7976931348623157e308
{-# INLINE m_huge #-}

-- | The largest 'Int' /x/ such that 2**(/x/-1) is approximately
-- representable as a 'Double'.
m_max_exp :: Int
m_max_exp = 1024

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

-- | The smallest 'Double' &#949; such that 1 + &#949; &#8800; 1.
m_epsilon :: Double
m_epsilon = encodeFloat (signif+1) expo - 1.0
    where (signif,expo) = decodeFloat (1.0::Double)

-- | @log(sqrt((2*pi)) / 2@
m_ln_sqrt_2_pi :: Double
m_ln_sqrt_2_pi = 0.9189385332046727417803297364056176398613974736377834128171
{-# INLINE m_ln_sqrt_2_pi #-}

-- | Positive infinity.
m_pos_inf :: Double
m_pos_inf = 1/0
{-# INLINE m_pos_inf #-}

-- | Negative infinity.
m_neg_inf :: Double
m_neg_inf = -1/0
{-# INLINE m_neg_inf #-}

-- | Not a number.
m_NaN :: Double
m_NaN = 0/0
{-# INLINE m_NaN #-}
