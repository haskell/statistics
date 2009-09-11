{-# LANGUAGE BangPatterns #-}
-- |
-- Module    : Statistics.Math
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--

module Statistics.Math
    (
      logGamma
    ) where

import Data.Array.Vector
import Statistics.Constants (m_epsilon, m_sqrt_2_pi)

logGamma :: Double -> Double
logGamma x
    | x > 18    = let y | x > 1 / m_epsilon = 0
                        | otherwise   = 1 / (x*x)
                      z = ((-5.95238095238e-4 * y + 7.936500793651e-4) * y -
                           2.7777777777778e-3) * y + 8.3333333333333e-2
                  in (x - 0.5) * log x - x + log m_sqrt_2_pi + z / x
    | x <= 0    = 1/0
    | otherwise = chebyshev alng 15 (z*2-1) + k + y
  where
    z :*: y
        | x > 4     = twiddle 3 (floor x - 1) 1
        | x < 3     = n2 $ twiddle 2 (floor x) (-1)
        | otherwise = x - 3 :*: 0
    twiddle a b d   = z :*: (log . foldlU f 1 . enumFromThenToU a (a+d) $ b)
        where f g i = g * (z + fromIntegral i)
              z     = x - fromIntegral (floor x)
    n2 (a :*: b)    = (a :*: -b)
    k               = 0.9574186990510627

data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Evaluate a series of Chebyshev polynomials.
chebyshev :: UArr Double        -- ^ Coefficients of the polynomials.
          -> Int                -- ^ Degree of the polynomial.
          -> Double             -- ^ Parameter of each function.
          -> Double
chebyshev a n x = fini . foldlU step (T 0 0 0) . enumFromThenToU n (n-1) $ 0
    where step (T u v w) k = T (x2 * u - v + indexU a k) u v
          fini (T u _ w)   = (u - w) / 2
          x2               = x * 2

alng :: UArr Double
alng = toU [  0.52854303698223459887
           ,  0.54987644612141411418
           ,  0.02073980061613665136
           , -0.00056916770421543842
           ,  0.00002324587210400169
           , -0.00000113060758570393
           ,  0.00000006065653098948
           , -0.00000000346284357770
           ,  0.00000000020624998806
           , -0.00000000001266351116
           ,  0.00000000000079531007
           , -0.00000000000005082077
           ,  0.00000000000000329187
           , -0.00000000000000021556
           ,  0.00000000000000001424
           , -0.00000000000000000095
           ]


lg2 :: Double -> Double
lg2 x = r
  where
    !r | x > 1e30 || x <= 0 = 1/0
       | x < 1.5 = a + c *
                   ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                   ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
        | x < 4   = (x - 2) *
                    ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                    ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
        | x < 12  = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                    ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
        | x > 5.1e5 = k
        | otherwise = k + x1 *
                      ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                      ((x2 + r4_4) * x2 + r4_3)
    a :*: b :*: c
        | x < 0.5   = -y :*: x + 1 :*: x
        | otherwise = 0  :*: x     :*: x - 1

    y      = log x
    k      = x * (y-1) - 0.5 * y + alr2pi
    alr2pi = 0.918938533204673

    x1 = 1 / x
    x2 = x1 * x1

    r1_0 = -2.66685511495; r1_1 = -24.4387534237; r1_2 = -21.9698958928
    r1_3 = 11.1667541262; r1_4 = 3.13060547623; r1_5 = 0.607771387771
    r1_6 = 11.9400905721; r1_7 = 31.4690115749; r1_8 = 15.2346874070

    r2_0 = -78.3359299449; r2_1 = -142.046296688; r2_2 = 137.519416416
    r2_3 = 78.6994924154; r2_4 = 4.16438922228; r2_5 = 47.0668766060
    r2_6 = 313.399215894; r2_7 = 263.505074721; r2_8 = 43.3400022514

    r3_0 = -2.12159572323; r3_1 = 2.30661510616; r3_2 = 2.74647644705
    r3_3 = -4.02621119975; r3_4 = -2.29660729780; r3_5 = -1.16328495004
    r3_6 = -1.46025937511; r3_7 = -2.42357409629; r3_8 = -5.70691009324

    r4_0 = 0.279195317918525; r4_1 = 0.4917317610505968;
    r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
    r4_4 = 6.012459259764103

-- $references
--
-- * Macleod, A.J. (1989) Algorithm AS 245: A robust and reliable
--   algorithm for the logarithm of the gamma function. /Journal of
--   the Royal Statistical Society, Series C (Applied Statistics)/
--   (38)2:397-402.
