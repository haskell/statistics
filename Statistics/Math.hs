{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- |
-- Module    : Statistics.Math
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
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
    -- ** Beta function
    , logBeta
    , incompleteBeta
    , incompleteBeta_
    , invIncompleteBeta
    -- ** Chebyshev polynomials
    -- $chebyshev
    , chebyshev
    , chebyshevBroucke
    -- ** Factorial
    , factorial
    , logFactorial
    -- ** Gamma function
    , logGamma
    , logGammaL
    , incompleteGamma
    , invIncompleteGamma
    -- ** Logarithm
    , log1p
    , log2
    -- ** Stirling's approximation
    , stirlingError
    , bd0
    -- * References
    -- $references
    ) where

import Data.Bits ((.&.), (.|.), shiftR)
import Data.Int (Int64)
import Data.Word (Word64)
import Statistics.Constants (m_epsilon, m_sqrt_2_pi, m_ln_sqrt_2_pi, m_NaN,
                             m_neg_inf, m_pos_inf)
import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (standard)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G


-- $chebyshev
--
-- A Chebyshev polynomial of the first kind is defined by the
-- following recurrence:
--
-- > t 0 _ = 1
-- > t 1 x = x
-- > t n x = 2 * x * t (n-1) x - t (n-2) x

data C = C {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Evaluate a Chebyshev polynomial of the first kind. Uses
-- Clenshaw's algorithm.
chebyshev :: (G.Vector v Double) =>
             Double      -- ^ Parameter of each function.
          -> v Double    -- ^ Coefficients of each polynomial term, in increasing order.
          -> Double
chebyshev x a = fini . G.foldr' step (C 0 0) . G.tail $ a
    where step k (C b0 b1) = C (k + x2 * b0 - b1) b0
          fini   (C b0 b1) = G.head a + x * b0 - b1
          x2               = x * 2
{-# INLINE chebyshev #-}

data B = B {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Evaluate a Chebyshev polynomial of the first kind. Uses Broucke's
-- ECHEB algorithm, and his convention for coefficient handling, and so
-- gives different results than 'chebyshev' for the same inputs.
chebyshevBroucke :: (G.Vector v Double) =>
             Double      -- ^ Parameter of each function.
          -> v Double    -- ^ Coefficients of each polynomial term, in increasing order.
          -> Double
chebyshevBroucke x = fini . G.foldr' step (B 0 0 0)
    where step k (B b0 b1 _) = B (k + x2 * b0 - b1) b0 b1
          fini   (B b0 _ b2) = (b0 - b2) * 0.5
          x2                 = x * 2
{-# INLINE chebyshevBroucke #-}

-- | Quickly compute the natural logarithm of /n/ @`choose`@ /k/, with
-- no checking.
logChooseFast :: Double -> Double -> Double
logChooseFast n k = -log (n + 1) - logBeta (n - k + 1) (k + 1)

-- | Compute the binomial coefficient /n/ @\``choose`\`@ /k/. For
-- values of /k/ > 30, this uses an approximation for performance
-- reasons.  The approximation is accurate to 12 decimal places in the
-- worst case
--
-- Example:
--
-- > 7 `choose` 3 == 35
choose :: Int -> Int -> Double
n `choose` k
    | k  > n         = 0
    | k' < 50        = U.foldl' go 1 . U.enumFromTo 1 $ k'
    | approx < max64 = fromIntegral . round64 $ approx
    | otherwise      = approx
  where
    k'             = min k (n-k)
    approx         = exp $ logChooseFast (fromIntegral n) (fromIntegral k')
                  -- Less numerically stable:
                  -- exp $ lg (n+1) - lg (k+1) - lg (n-k+1)
                  --   where lg = logGamma . fromIntegral
    go a i         = a * (nk + j) / j
        where j    = fromIntegral i :: Double
    nk             = fromIntegral (n - k')
    max64          = fromIntegral (maxBound :: Int64)
    round64 x      = round x :: Int64

data F = F {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | Compute the factorial function /n/!.  Returns &#8734; if the
-- input is above 170 (above which the result cannot be represented by
-- a 64-bit 'Double').
factorial :: Int -> Double
factorial n
    | n < 0     = error "Statistics.Math.factorial: negative input"
    | n <= 1    = 1
    | n <= 14   = fini . U.foldl' goLong (F 1 1) $ ns
    | otherwise = U.foldl' goDouble 1 ns
    where goDouble t k = t * fromIntegral k
          goLong (F z x) _ = F (z * x') x'
              where x' = x + 1
          fini (F z _) = fromIntegral z
          ns = U.enumFromTo 2 n

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

-- | Compute the normalized lower incomplete gamma function
-- &#947;(/s/,/x/). Normalization means that
-- &#947;(/s/,&#8734;)=1. Uses Algorithm AS 239 by Shea.
incompleteGamma :: Double       -- ^ /s/
                -> Double       -- ^ /x/
                -> Double
incompleteGamma p x
    | x < 0 || p <= 0 = m_pos_inf
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



-- Adapted from Numerical Recipes §6.2.1

-- | Inverse incomplete gamma function. It's approximately inverse of
--   'incompleteGamma' for the same /s/. So following equality
--   approximately holds:
--
-- > invIncompleteGamma s . incompleteGamma s = id
--
--   For @invIncompleteGamma s p@ /s/ must be positive and /p/ must be
--   in [0,1] range.
invIncompleteGamma :: Double -> Double -> Double
invIncompleteGamma a p
  | a <= 0         = 
      error $ "Statistics.Math.invIncompleteGamma: a must be positive. Got: " ++ show a
  | p < 0 || p > 1 = 
      error $ "Statistics.Math.invIncompleteGamma: p must be in [0,1] range. Got: " ++ show p
  | p == 0         = 0
  | p == 1         = 1 / 0
  | otherwise      = loop 0 guess
  where
    -- Solve equation γ(a,x) = p using Halley method
    loop :: Int -> Double -> Double
    loop i x
      | i >= 12   = x
      | otherwise =
         let 
           -- Value of γ(a,x) - p
           f    = incompleteGamma a x - p
           -- dγ(a,x)/dx
           f'   | a > 1     = afac * exp( -(x - a1) + a1 * (log x - lna1))
                | otherwise = exp( -x + a1 * log x - gln)
           u    = f / f'
           -- Halley correction to Newton-Rapson step
           corr = u * (a1 / x - 1)
           dx   = u / (1 - 0.5 * min 1.0 corr)
           -- New approximation to x
           x'   | x < dx    = 0.5 * x -- Do not go below 0
                | otherwise = x - dx
         in if abs dx < eps * x'
            then x'
            else loop (i+1) x'
    -- Calculate inital guess for root
    guess
      -- 
      | a > 1   =
         let t  = sqrt $ -2 * log(if p < 0.5 then p else 1 - p)
             x1 = (2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t
             x2 = if p < 0.5 then -x1 else x1
         in max (1e-3) (a * (1 - 1/(9*a) - x2 / (3 * sqrt a)) ** 3)
      -- For a <= 1 use following approximations:
      --   γ(a,1) ≈ 0.253a + 0.12a²
      --
      --   γ(a,x) ≈ γ(a,1)·x^a                               x <  1
      --   γ(a,x) ≈ γ(a,1) + (1 - γ(a,1))(1 - exp(1 - x))    x >= 1
      | otherwise =
         let t = 1 - a * (0.253 + a*0.12)
         in if p < t
            then (p / t) ** (1 / a)
            else 1 - log( 1 - (p-t) / (1-t))
    -- Constants
    a1   = a - 1
    lna1 = log a1
    afac = exp( a1 * (lna1 - 1) - gln )
    gln  = logGamma a
    eps  = 1e-8



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
    | x <= 0    = m_pos_inf
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
    | x <= 0    = m_pos_inf
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
    | x < 10    = m_NaN
    | x < big   = chebyshevBroucke (t * t * 2 - 1) coeffs / x
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
              -0.3399615005417721944303330599666e-15,
               0.2683181998482698748957538846666e-17
             ]

-- | Compute the natural logarithm of the beta function.
logBeta :: Double -> Double -> Double
logBeta a b
    | p < 0     = m_NaN
    | p == 0    = m_pos_inf
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

-- | Regularized incomplete beta function. Uses algorithm AS63 by
--   Majumder abd Bhattachrjee.
incompleteBeta :: Double -- ^ /p/ > 0
               -> Double -- ^ /q/ > 0
               -> Double -- ^ /x/, must lie in [0,1] range
               -> Double
incompleteBeta p q = incompleteBeta_ (logBeta p q) p q

-- | Regularized incomplete beta function. Same as 'incompleteBeta'
-- but also takes value of lo
incompleteBeta_ :: Double -- ^ logarithm of beta function
                -> Double -- ^ /p/ > 0
                -> Double -- ^ /q/ > 0
                -> Double -- ^ /x/, must lie in [0,1] range
                -> Double
incompleteBeta_ beta p q x
  | p <= 0 || q <= 0 = error "p <= 0 || q <= 0"
  | x <  0 || x >  1 = error "x <  0 || x >  1"
  | x == 0 || x == 1 = x
  | p >= (p+q) * x   = incompleteBetaWorker beta p q x
  | otherwise        = 1 - incompleteBetaWorker beta q p (1 - x)

-- Worker for incomplete beta function. It is separate function to
-- avoid confusion with parameter during parameter swapping
incompleteBetaWorker :: Double -> Double -> Double -> Double -> Double
incompleteBetaWorker beta p q x = loop (p+q) (truncate $ q + cx * (p+q) :: Int) 1 1 1
  where
    -- Constants
    eps = 1e-15
    cx  = 1 - x
    -- Loop
    loop psq ns ai term betain
      | done      = betain' * exp( p * log x + (q - 1) * log cx - beta) / p
      | otherwise = loop psq' (ns - 1) (ai + 1) term' betain'
      where
        -- New values
        term'   = term * fact / (p + ai)
        betain' = betain + term'
        fact | ns >  0   = (q - ai) * x/cx
             | ns == 0   = (q - ai) * x
             | otherwise = psq * x
        -- Iterations are complete
        done = db <= eps && db <= eps*betain' where db = abs term'
        psq' = if ns < 0 then psq + 1 else psq

-- | Compute inverse of regularized incomplete beta function. Uses
-- initial approximation from AS109 and Halley method to solve equation.
invIncompleteBeta :: Double     -- ^ /p/
                  -> Double     -- ^ /q/
                  -> Double     -- ^ /a/
                  -> Double
invIncompleteBeta p q a
  | p <= 0 || q <= 0 = error "p <= 0 || q <= 0"
  | a <  0 || a >  1 = error "bad a"
  | a == 0 || a == 1 = a
  | a > 0.5          = 1 - invIncompleteBetaWorker (logBeta p q) q p (1 - a)
  | otherwise        = invIncompleteBetaWorker (logBeta p q) p q a

invIncompleteBetaWorker :: Double -> Double -> Double -> Double -> Double
invIncompleteBetaWorker beta p q a = loop (0::Int) guess
  where
    p1 = p - 1
    q1 = q - 1
    -- Solve equation using Halley method
    loop !i !x
      | x == 0 || x == 1             = x
      | i >= 10                      = x
      | abs dx <= 16 * m_epsilon * x = x
      | otherwise                    = loop (i+1) x'
      where
        f   = incompleteBeta_ beta p q x - a
        f'  = exp $ p1 * log x + q1 * log (1 - x) - beta
        u   = f / f'
        dx  = u / (1 - 0.5 * min 1 (u * (p1 / x - q1 / (1 - x))))
        x'  | z < 0     = x / 2
            | z > 1     = (x + 1) / 2
            | otherwise = z
            where z = x - dx
    -- Calculate initial guess
    guess 
      | p > 1 && q > 1 = 
          let rr = (y*y - 3) / 6
              ss = 1 / (2*p - 1)
              tt = 1 / (2*q - 1)
              hh = 2 / (ss + tt)
              ww = y * sqrt(hh + rr) / hh - (tt - ss) * (rr + 5/6 - 2 / (3 * hh))
          in p / (p + q * exp(2 * ww))
      | t'  <= 0  = 1 - exp( (log((1 - a) * q) + beta) / q )
      | t'' <= 1  = exp( (log(a * p) + beta) / p )
      | otherwise = 1 - 2 / (t'' + 1)
      where
        r   = sqrt ( - log ( a * a ) )
        y   = r - ( 2.30753 + 0.27061 * r )
                   / ( 1.0 + ( 0.99229 + 0.04481 * r ) * r )
        t   = 1 / (9 * q)
        t'  = 2 * q * (1 - t + y * sqrt t) ** 3
        t'' = (4*p + 2*q - 2) / t'
        
            

-- | Compute the natural logarithm of 1 + @x@.  This is accurate even
-- for values of @x@ near zero, where use of @log(1+x)@ would lose
-- precision.
log1p :: Double -> Double
log1p x
    | x == 0               = 0
    | x == -1              = m_neg_inf
    | x < -1               = m_NaN
    | x' < m_epsilon * 0.5 = x
    | (x >= 0 && x < 1e-8) || (x >= -1e-9 && x < 0)
                           = x * (1 - x * 0.5)
    | x' < 0.375           = x * (1 - x * chebyshevBroucke (x / 0.375) coeffs)
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
              -0.10324619158271569595141333961932e-15
             ]

-- | Calculate the error term of the Stirling approximation.  This is
-- only defined for non-negative values.
--
-- > stirlingError @n@ = @log(n!) - log(sqrt(2*pi*n)*(n/e)^n)
stirlingError :: Double -> Double
stirlingError n 
  | n <= 15.0   = case properFraction (n+n) of
                    (i,0) -> sfe `U.unsafeIndex` i
                    _     -> logGamma (n+1.0) - (n+0.5) * log n + n -
                             m_ln_sqrt_2_pi
  | n > 500     = (s0-s1/nn)/n
  | n > 80      = (s0-(s1-s2/nn)/nn)/n
  | n > 35      = (s0-(s1-(s2-s3/nn)/nn)/nn)/n
  | otherwise   = (s0-(s1-(s2-(s3-s4/nn)/nn)/nn)/nn)/n
  where
    nn = n*n
    s0 = 0.083333333333333333333        -- 1/12
    s1 = 0.00277777777777777777778      -- 1/360
    s2 = 0.00079365079365079365079365   -- 1/1260
    s3 = 0.000595238095238095238095238  -- 1/1680
    s4 = 0.0008417508417508417508417508 -- 1/1188
    sfe = U.fromList [ 0.0, 
                0.1534264097200273452913848,   0.0810614667953272582196702,
                0.0548141210519176538961390,   0.0413406959554092940938221,
                0.03316287351993628748511048,  0.02767792568499833914878929,
                0.02374616365629749597132920,  0.02079067210376509311152277,
                0.01848845053267318523077934,  0.01664469118982119216319487,
                0.01513497322191737887351255,  0.01387612882307074799874573,
                0.01281046524292022692424986,  0.01189670994589177009505572,
                0.01110455975820691732662991,  0.010411265261972096497478567,
                0.009799416126158803298389475, 0.009255462182712732917728637,
                0.008768700134139385462952823, 0.008330563433362871256469318,
                0.007934114564314020547248100, 0.007573675487951840794972024,
                0.007244554301320383179543912, 0.006942840107209529865664152,
                0.006665247032707682442354394, 0.006408994188004207068439631,
                0.006171712263039457647532867, 0.005951370112758847735624416,
                0.005746216513010115682023589, 0.005554733551962801371038690 ]


-- | Evaluate the deviance term @x log(x/np) + np - x@.
bd0 :: Double                   -- ^ @x@
    -> Double                   -- ^ @np@
    -> Double 
bd0 x np 
  | isInfinite x || isInfinite np || np == 0 = m_NaN
  | abs x_np >= 0.1*(x+np)                   = x * log (x/np) - x_np
  | otherwise                                = loop 1 (ej0*vv) s0
  where 
    x_np = x - np
    v    = x_np / (x+np)
    s0   = x_np * v
    ej0  = 2*x*v
    vv   = v*v
    loop j ej s = case s + ej/(2*j+1) of
                    s' | s' == s   -> s'
                       | otherwise -> loop (j+1) (ej*vv) s'

-- | /O(log n)/ Compute the logarithm in base 2 of the given value.
log2 :: Int -> Int
log2 v0
    | v0 <= 0   = error "Statistics.Math.log2: invalid input"
    | otherwise = go 5 0 v0
  where
    go !i !r !v | i == -1        = r
                | v .&. b i /= 0 = let si = U.unsafeIndex sv i
                                   in go (i-1) (r .|. si) (v `shiftR` si)
                | otherwise      = go (i-1) r v
    b = U.unsafeIndex bv
    !bv = U.fromList [0x2, 0xc, 0xf0, 0xff00, 0xffff0000, 0xffffffff00000000]
    !sv = U.fromList [1,2,4,8,16,32]

-- $references
--
-- * Broucke, R. (1973) Algorithm 446: Ten subroutines for the
--   manipulation of Chebyshev series. /Communications of the ACM/
--   16(4):254&#8211;256.  <http://doi.acm.org/10.1145/362003.362037>
--
-- * Clenshaw, C.W. (1962) Chebyshev series for mathematical
--   functions. /National Physical Laboratory Mathematical Tables 5/,
--   Her Majesty's Stationery Office, London.
--
-- * Lanczos, C. (1964) A precision approximation of the gamma
--   function.  /SIAM Journal on Numerical Analysis B/
--   1:86&#8211;96. <http://www.jstor.org/stable/2949767>
--
-- * Loader, C. (2000) Fast and Accurate Computation of Binomial
--   Probabilities. <http://projects.scipy.org/scipy/raw-attachment/ticket/620/loader2000Fast.pdf>
--
-- * Macleod, A.J. (1989) Algorithm AS 245: A robust and reliable
--   algorithm for the logarithm of the gamma function.
--   /Journal of the Royal Statistical Society, Series C (Applied Statistics)/
--   38(2):397&#8211;402. <http://www.jstor.org/stable/2348078>
--
-- * Shea, B. (1988) Algorithm AS 239: Chi-squared and incomplete
--   gamma integral. /Applied Statistics/
--   37(3):466&#8211;473. <http://www.jstor.org/stable/2347328>
--
-- * K. L. Majumder, G. P. Bhattacharjee (1973) Algorithm AS 63: The
--   Incomplete Beta Integral. /Journal of the Royal Statistical
--   Society. Series C (Applied Statistics)/ Vol. 22, No. 3 (1973),
--   pp. 409-411. <http://www.jstor.org/pss/2346797>
--
-- * K. L. Majumder, G. P. Bhattacharjee (1973) Algorithm AS 64:
--   Inverse of the Incomplete Beta Function Ratio. /Journal of the
--   Royal Statistical Society. Series C (Applied Statistics)/
--   Vol. 22, No. 3 (1973), pp. 411-414
--   <http://www.jstor.org/pss/2346798>
--
-- * G. W. Cran, K. J. Martin and G. E. Thomas (1977) Remark AS R19
--   and Algorithm AS 109: A Remark on Algorithms: AS 63: The
--   Incomplete Beta Integral AS 64: Inverse of the Incomplete Beta
--   Function Ratio. /Journal of the Royal Statistical Society. Series
--   C (Applied Statistics)/ Vol. 26, No. 1 (1977), pp. 111-114
--   <http://www.jstor.org/pss/2346887>
