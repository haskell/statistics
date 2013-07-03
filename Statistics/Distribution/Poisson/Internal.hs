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
      probability, poissonEntropy
    ) where

import Data.List(unfoldr)
import Numeric.MathFunctions.Constants (m_sqrt_2_pi, m_tiny, m_epsilon)
import Numeric.SpecFunctions           (logGamma, stirlingError, choose)
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

-- | Compute entropy using Theorem 1 from "Sharp Bounds on the Entropy
-- of the Poisson Law" by José A. Adell, Alberto Lekuona, and Yaming
-- Yu.  This is highly robust for lambda <= 1.
alyThm1 :: Double -> Double
alyThm1 lambda =
  sum (takeWhile (\x -> abs x >= m_epsilon * lll) alySeries) + lll
  where lll = lambda * (1 - log lambda)
        alySeries =
          [ alyc k * exp (fromIntegral k * log lambda 
                          - logGamma (fromIntegral k))
          | k <- [2..] ]

alyc :: Int -> Double
alyc k =
  sum [ parity j * choose (k-1) j * log (fromIntegral j+1) | j <- [0..k-1] ]
  where parity j
          | even (k-j) = -1
          | otherwise  = 1
                         
                         
-- | Compute entropy using Theorem 2 from the same paper. This is highly
-- accurate for large lambda.
alyThm2 :: Double -> Double
alyThm2 lambda =
  1.4189385332046727 + 0.5 * log lambda +
  (sum $ map (uncurry (*)) (zip powers coefficients))
  where powers = unfoldr (\x -> Just (x/lambda,x/lambda)) 1
        coefficients =
          [1/12, 1/24, 19/360, 9/80, 863/2520, -123365/1008, -3023561/2520,
           -808157/720, -984451/5940, -1151/440, -1/1716]

-- | Compute entropy by brute force. Limits on floating point precision
-- cause this to underestimate by quite a bit, but for intermediate values
-- of lambda it's the best we can do.
bruteforce :: Double -> Double
bruteforce lambda =   
  negate . sum $
  takeWhile (< negate m_epsilon * lambda) $
  dropWhile (not . (< negate m_epsilon * lambda)) $
  [ let x = probability lambda k in x * log x | k <- [0..]]

-- | Compute the entropy of a poisson distribution. Use 'alyThm1' for
-- small lambda, 'alyThm2' for large lambda, and 'bruteforce' plus a
-- linear spline for intermediate lambda.
poissonEntropy :: Double -> Double
poissonEntropy lambda
  | lambda == 0 = 0
  | lambda <= 1 = alyThm1 lambda
  | lambda >= 4.671691395381868 = alyThm2 lambda
  | otherwise = bruteforce lambda 
                - 7.314355647771469e-2 * lambda
                + 0.3417041234245674
