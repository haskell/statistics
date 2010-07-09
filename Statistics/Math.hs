{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Math
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Mathematical functions for statistics.

module Statistics.Math
    (
    -- * Functions
      choose
    -- ** Chebyshev polynomials
    , chebyshev
    -- ** Logarithm
    , log1p
    -- ** Factorial
    , factorial
    , logFactorial
    -- ** Gamma
    , incompleteGamma
    , logGamma
    , logGammaL
    , logGammaCorrection
    -- ** Beta
    , logBeta
    -- * References
    -- $references
    ) where

import Data.Vector.Generic ((!))
import Data.Word (Word64)
import Statistics.Constants (m_epsilon, m_sqrt_2_pi, m_ln_sqrt_2_pi)
import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (standard)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

data C = C {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Evaluate a series of Chebyshev polynomials. Uses Clenshaw's
-- algorithm.
chebyshev :: (G.Vector v Double) =>
             Double      -- ^ Parameter of each function.
          -> v Double    -- ^ Coefficients of each polynomial term, in increasing order.
          -> Double
chebyshev x a = fini . U.foldl' step (C 0 0) $ U.enumFromStepN (len - 1) (-1) (len - 1)
    where step (C b1 b2) k = C ((a ! k) + x2 * b1 - b2) b1
          fini (C b1 b2)   = (a ! 0) + x * b1 - b2
          x2               = x * 2
          len              = G.length a
{-# INLINE chebyshev #-}

data CB = CB {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- Evaluate a series of Chebyshev polynomials. Uses Broucke's
-- adaptation of Clenshaw's aapproach.
--
-- This implementation omits the final division by 2 added by Broucke,
-- so that the results may be compared to 'chebyshev' above.
chebyshevBroucke :: (G.Vector v Double) =>
        Double -> v Double -> Double
chebyshevBroucke x a
  | n < 1 || n > 1000 = 0/0
  | x < -1.1 || x > 1.1 = 0/0
  | otherwise = fini . U.foldl' step (CH 0 0 0) $ U.enumFromStepN (n - 1) (-1) n
 where
  step (CH b0 b1 b2) k = CH (x2 * b1 - b2 + (a ! k)) b0 b1
  fini (CH b0 _ b2)    = b0 - b2
  n                    = G.length a
  x2                   = x * 2

-- | The binomial coefficient. This function could be slow for large
-- /n/ and /k/. It could be useful to use some kind of approximation.
--
-- > 7 `choose` 3 == 35
choose :: Int -> Int -> Double
n `choose` k
    | k > n     = 0
    | k < 30    = U.foldl' go 1 . U.enumFromTo 1 $ k'
    | otherwise = exp $ lg (n+1) - lg (k+1) - lg (n-k+1)
    where go a i = a * (nk + j) / j
              where j = fromIntegral i :: Double
          k' | n_k < k   = n_k
             | otherwise = k
             where n_k   = n - k
          nk = fromIntegral (n - k')
          lg = logGamma . fromIntegral
{-# INLINE choose #-}

data F = F {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | Compute the factorial function /n/!.  Returns &#8734; if the
-- input is above 170 (above which the result cannot be represented by
-- a 64-bit 'Double').
factorial :: Int -> Double
factorial n
    | n < 0     = error "Statistics.Math.factorial: negative input"
    | n <= 1    = 1
    | n <= 14   = fini . U.foldl' goLong (F 1 1) $ ns
    | otherwise = U.foldl' goDouble 1 $ ns
    where goDouble t k = t * fromIntegral k
          goLong (F z x) _ = F (z * x') x'
              where x' = x + 1
          fini (F z _) = fromIntegral z
          ns = U.enumFromTo 2 n
{-# INLINE factorial #-}

-- | Compute the natural logarithm of the factorial function.  Gives
-- 16 decimal digits of precision.
logFactorial :: Int -> Double
logFactorial n
    | n <= 14   = log (factorial n)
    | otherwise = (x - 0.5) * log x - x + 9.1893853320467e-1 + z / x
    where x = fromIntegral (n + 1)
          y = 1 / (x * x)
          z = ((-(5.95238095238e-4 * y) + 7.936500793651e-4) * y -
               2.7777777777778e-3) * y + 8.3333333333333e-2
{-# INLINE logFactorial #-}

-- | Compute the normalized lower incomplete gamma function
-- &#947;(/s/,/x/). Normalization means that
-- &#947;(&#8734;,/x/)=1. Uses Algorithm AS 239 by Shea.
incompleteGamma :: Double       -- ^ /s/
                -> Double       -- ^ /x/
                -> Double
incompleteGamma x p
    | x < 0 || p <= 0 = 1/0
    | x == 0          = 0
    | p >= 1000       = norm (3 * sqrt p * ((x/p) ** (1/3) + 1/(9*p) - 1))
    | x >= 1e8        = 1
    | x <= 1 || x < p = let a = p * log x - x - logGamma (p + 1)
                            g = a + log (pearson p 1 1)
                        in if g > limit then exp g else 0
    | otherwise       = let g = p * log x - x - logGamma p + log cf
                        in if g > limit then 1 - exp g else 1
  where
    norm = cumulative standard
    pearson !a !c !g
        | c' <= tolerance = g'
        | otherwise       = pearson a' c' g'
        where a' = a + 1
              c' = c * x / a'
              g' = g + c'
    cf = let a = 1 - p
             b = a + x + 1
             p3 = x + 1
             p4 = x * b
         in contFrac a b 0 1 x p3 p4 (p3/p4)
    contFrac !a !b !c !p1 !p2 !p3 !p4 !g
        | abs (g - rn) <= min tolerance (tolerance * rn) = g
        | otherwise = contFrac a' b' c' (f p3) (f p4) (f p5) (f p6) rn
        where a' = a + 1
              b' = b + 2
              c' = c + 1
              an = a' * c'
              p5 = b' * p3 - an * p1
              p6 = b' * p4 - an * p2
              rn = p5 / p6
              f n | abs p5 > overflow = n / overflow
                  | otherwise         = n
    limit     = -88
    tolerance = 1e-14
    overflow  = 1e37

-- Adapted from http://people.sc.fsu.edu/~burkardt/f_src/asa245/asa245.html

-- | Compute the logarithm of the gamma function &#915;(/x/).  Uses
-- Algorithm AS 245 by Macleod.
--
-- Gives an accuracy of 10&#8211;12 significant decimal digits, except
-- for small regions around /x/ = 1 and /x/ = 2, where the function
-- goes to zero.  For greater accuracy, use 'logGammaL'.
--
-- Returns &#8734; if the input is outside of the range (0 < /x/
-- &#8804; 1e305).
logGamma :: Double -> Double
logGamma x
    | x <= 0    = 1/0
    | x < 1.5   = a + c *
                  ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                  ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
    | x < 4     = (x - 2) *
                  ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                  ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
    | x < 12    = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                  ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
    | x > 5.1e5 = k
    | otherwise = k + x1 *
                  ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                  ((x2 + r4_4) * x2 + r4_3)
  where
    (a , b , c)
        | x < 0.5   = (-y , x + 1 , x)
        | otherwise = (0  , x     , x - 1)

    y      = log x
    k      = x * (y-1) - 0.5 * y + alr2pi
    alr2pi = 0.918938533204673

    x1 = 1 / x
    x2 = x1 * x1

    r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
    r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
    r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070

    r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
    r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
    r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514

    r3_0 =  -2.12159572323e5; r3_1 =    2.30661510616e5; r3_2 =   2.74647644705e4
    r3_3 =  -4.02621119975e4; r3_4 =   -2.29660729780e3; r3_5 =  -1.16328495004e5
    r3_6 =  -1.46025937511e5; r3_7 =   -2.42357409629e4; r3_8 =  -5.70691009324e2

    r4_0 = 0.279195317918525;  r4_1 = 0.4917317610505968;
    r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
    r4_4 = 6.012459259764103

data L = L {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the logarithm of the gamma function, &#915;(/x/).  Uses a
-- Lanczos approximation.
--
-- This function is slower than 'logGamma', but gives 14 or more
-- significant decimal digits of accuracy, except around /x/ = 1 and
-- /x/ = 2, where the function goes to zero.
--
-- Returns &#8734; if the input is outside of the range (0 < /x/
-- &#8804; 1e305).
logGammaL :: Double -> Double
logGammaL x
    | x <= 0    = 1/0
    | otherwise = fini . U.foldl' go (L 0 (x+7)) $ a
    where fini (L l _) = log (l+a0) + log m_sqrt_2_pi - x65 + (x-0.5) * log x65
          go (L l t) k = L (l + k / t) (t-1)
          x65 = x + 6.5
          a0  = 0.9999999999995183
          a   = U.fromList [ 0.1659470187408462e-06
                           , 0.9934937113930748e-05
                           , -0.1385710331296526
                           , 12.50734324009056
                           , -176.6150291498386
                           , 771.3234287757674
                           , -1259.139216722289
                           , 676.5203681218835
                           ]

-- | Compute the log gamma correction factor for @x@ &#8805; 10.  This
-- correction factor is suitable for an alternate (but less
-- numerically accurate) definition of 'logGamma':
--
-- >lgg x = 0.5 * log(2*pi) + (x-0.5) * log x - x + logGammaCorrection x
logGammaCorrection :: Double -> Double
logGammaCorrection x
    | x < 10    = 0/0
    | x < big   = chebyshev (t * t * 2 - 1) coeffs / (x * 2)
    | otherwise = 1 / (x * 12)
  where
    big    = 94906265.62425156
    t      = 10 / x
    coeffs = U.fromList [
               0.1666389480451863247205729650822e+0,
              -0.1384948176067563840732986059135e-4,
               0.9810825646924729426157171547487e-8,
              -0.1809129475572494194263306266719e-10,
               0.6221098041892605227126015543416e-13,
              -0.3399615005417721944303330599666e-15
             ]

-- | Compute the natural logarithm of the beta function.
logBeta :: Double -> Double -> Double
logBeta a b
    | p < 0     = 0/0
    | p == 0    = 1/0
    | p >= 10   = log q * (-0.5) + m_ln_sqrt_2_pi + logGammaCorrection p + c +
                  (p - 0.5) * log ppq + q * log1p(-ppq)
    | q >= 10   = logGamma p + c + p - p * log pq + (q - 0.5) * log1p(-ppq)
    | otherwise = logGamma p + logGamma q - logGamma pq
    where
      p   = min a b
      q   = max a b
      ppq = p / pq
      pq  = p + q
      c   = logGammaCorrection q - logGammaCorrection pq

-- | Compute the natural logarithm of 1 + @x@.  This is accurate even
-- for values of @x@ near zero, where use of @log(1+x)@ would lose
-- precision.
log1p :: Double -> Double
log1p x
    | x == 0               = 0
    | x == -1              = -1/0
    | x < -1               = 0/0
    | x' < m_epsilon * 0.5 = x
    | (x >= 0 && x < 1e-8) || (x >= -1e-9 && x < 0)
                           = x * (1 - x * 0.5)
    | x' < 0.375           = x * (1 - x * chebyshev (x / 0.375) coeffs * 0.5)
    | otherwise            = log (1 + x)
  where
    x' = abs x
    coeffs = U.fromList [
               0.10378693562743769800686267719098e+1,
              -0.13364301504908918098766041553133e+0,
               0.19408249135520563357926199374750e-1,
              -0.30107551127535777690376537776592e-2,
               0.48694614797154850090456366509137e-3,
              -0.81054881893175356066809943008622e-4,
               0.13778847799559524782938251496059e-4,
              -0.23802210894358970251369992914935e-5,
               0.41640416213865183476391859901989e-6,
              -0.73595828378075994984266837031998e-7,
               0.13117611876241674949152294345011e-7,
              -0.23546709317742425136696092330175e-8,
               0.42522773276034997775638052962567e-9,
              -0.77190894134840796826108107493300e-10,
               0.14075746481359069909215356472191e-10,
              -0.25769072058024680627537078627584e-11,
               0.47342406666294421849154395005938e-12,
              -0.87249012674742641745301263292675e-13,
               0.16124614902740551465739833119115e-13,
              -0.29875652015665773006710792416815e-14,
               0.55480701209082887983041321697279e-15,
              -0.10324619158271569595141333961932e-15,
               0.19250239203049851177878503244868e-16,
              -0.35955073465265150011189707844266e-17,
               0.67264542537876857892194574226773e-18,
              -0.12602624168735219252082425637546e-18,
               0.23644884408606210044916158955519e-19,
              -0.44419377050807936898878389179733e-20,
               0.83546594464034259016241293994666e-21,
              -0.15731559416479562574899253521066e-21,
               0.29653128740247422686154369706666e-22,
              -0.55949583481815947292156013226666e-23,
               0.10566354268835681048187284138666e-23,
              -0.19972483680670204548314999466666e-24,
               0.37782977818839361421049855999999e-25,
              -0.71531586889081740345038165333333e-26,
               0.13552488463674213646502024533333e-26,
              -0.25694673048487567430079829333333e-27,
               0.48747756066216949076459519999999e-28,
              -0.92542112530849715321132373333333e-29,
               0.17578597841760239233269760000000e-29,
              -0.33410026677731010351377066666666e-30,
               0.63533936180236187354180266666666e-31
             ]

-- $references
--
-- * Broucke, R. (1973) Algorithm 446: Ten subroutines for the
--   manipulation of Chebyshev series. /Communications of the ACM/
--   16(4):254&#8211;256.  <http://doi.acm.org/10.1145/362003.362037>

-- * Clenshaw, C.W. (1962) Chebyshev series for mathematical
--   functions. /National Physical Laboratory Mathematical Tables 5/,
--   Her Majesty's Stationery Office, London.
--
-- * Lanczos, C. (1964) A precision approximation of the gamma
--   function.  /SIAM Journal on Numerical Analysis B/
--   1:86&#8211;96. <http://www.jstor.org/stable/2949767>
--
-- * Macleod, A.J. (1989) Algorithm AS 245: A robust and reliable
--   algorithm for the logarithm of the gamma function.
--   /Journal of the Royal Statistical Society, Series C (Applied Statistics)/
--   38(2):397&#8211;402. <http://www.jstor.org/stable/2348078>
--
-- * Shea, B. (1988) Algorithm AS 239: Chi-squared and incomplete
--   gamma integral. /Applied Statistics/
--   37(3):466&#8211;473. <http://www.jstor.org/stable/2347328>
