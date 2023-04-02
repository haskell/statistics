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

import Data.List (unfoldr)
import Numeric.MathFunctions.Constants (m_sqrt_2_pi, m_tiny, m_epsilon)
import Numeric.SpecFunctions (logGamma, stirlingError {-, choose, logFactorial -})
import Numeric.SpecFunctions.Extra (bd0)

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

-- -- | Compute entropy using Theorem 1 from "Sharp Bounds on the Entropy
-- -- of the Poisson Law".  This function is unused because 'directEntropy'
-- -- is just as accurate and is faster by about a factor of 4.
-- alyThm1 :: Double -> Double
-- alyThm1 lambda =
--   sum (takeWhile (\x -> abs x >= m_epsilon * lll) alySeries) + lll
--   where lll = lambda * (1 - log lambda)
--         alySeries =
--           [ alyc k * exp (fromIntegral k * log lambda - logFactorial k)
--           | k <- [2..] ]

-- alyc :: Int -> Double
-- alyc k =
--   sum [ parity j * choose (k-1) j * log (fromIntegral j+1) | j <- [0..k-1] ]
--   where parity j
--           | even (k-j) = -1
--           | otherwise  = 1

-- | Returns [x, x^2, x^3, x^4, ...]
powers :: Double -> [Double]
powers x = unfoldr (\y -> Just (y*x,y*x)) 1

-- | Returns an upper bound according to theorem 2 of "Sharp Bounds on
-- the Entropy of the Poisson Law"
alyThm2Upper :: Double -> [Double] -> Double
alyThm2Upper lambda coefficients =
  1.4189385332046727 + 0.5 * log lambda +
  zipCoefficients lambda coefficients

-- | Returns the average of the upper and lower bounds according to
-- theorem 2.
alyThm2 :: Double -> [Double] -> [Double] -> Double
alyThm2 lambda upper lower =
  alyThm2Upper lambda upper + 0.5 * (zipCoefficients lambda lower)

zipCoefficients :: Double -> [Double] -> Double
zipCoefficients lambda coefficients =
  (sum $ map (uncurry (*)) (zip (powers $ recip lambda) coefficients))

-- Mathematica code deriving the coefficients below:
--
-- poissonMoment[0, s_] := 1
-- poissonMoment[1, s_] := 0
-- poissonMoment[k_, s_] :=
--   Sum[s * Binomial[k - 1, j] * poissonMoment[j, s], {j, 0, k - 2}]
--
-- upperSeries[m_]  :=
--  Distribute[Integrate[
--    Sum[(-1)^(j - 1) *
--      poissonMoment[j, \[Lambda]] / (j * (j - 1)* \[Lambda]^j),
--     {j, 3, 2 m - 1}],
--    \[Lambda]]]
--
-- lowerSeries[m_] :=
--  Distribute[Integrate[
--    poissonMoment[
--      2 m + 2, \[Lambda]] / ((2 m +
--         1)*\[Lambda]^(2 m + 2)), \[Lambda]]]
--
-- upperBound[m_] := upperSeries[m] + (Log[2*Pi*\[Lambda]] + 1)/2
--
-- lowerBound[m_] := upperBound[m] + lowerSeries[m]

upperCoefficients4 :: [Double]
upperCoefficients4 = [1/12, 1/24, -103/180, -13/40, -1/210]

lowerCoefficients4 :: [Double]
lowerCoefficients4 = [0,0,0, -105/4, -210, -2275/18, -167/21, -1/72]

upperCoefficients6 :: [Double]
upperCoefficients6 = [1/12, 1/24, 19/360, 9/80, -38827/2520,
                      -74855/1008, -73061/2520, -827/720, -1/990]

lowerCoefficients6 :: [Double]
lowerCoefficients6 = [0,0,0,0,0, -3465/2, -45045, -466235/4, -531916/9,
                      -56287/10, -629/11, -1/156]

upperCoefficients8 :: [Double]
upperCoefficients8 = [1/12, 1/24, 19/360, 9/80, 863/2520, 1375/1008,
                      -3023561/2520, -15174047/720, -231835511/5940,
                      -18927611/1320, -58315591/60060, -23641/3640,
                      -1/2730]

lowerCoefficients8 :: [Double]
lowerCoefficients8 = [0,0,0,0,0,0,0, -2027025/8, -15315300, -105252147,
                      -178127950, -343908565/4, -10929270, -3721149/14,
                      -7709/15, -1/272]

upperCoefficients10 :: [Double]
upperCoefficients10 = [1/12, 1/24, 19/360, 9,80, 863/2520, 1375/1008,
                       33953/5040, 57281/1440, -2271071617/11880,
                       -1483674219/176, -31714406276557/720720,
                       -7531072742237/131040, -1405507544003/65520,
                       -21001919627/10080, -1365808297/36720,
                       -26059/544, -1/5814]

lowerCoefficients10 :: [Double]
lowerCoefficients10 = [0,0,0,0,0,0,0,0,0,-130945815/2, -7638505875,
                       -438256243425/4, -435477637540, -3552526473925/6,
                       -857611717105/3, -545654955967/12, -5794690528/3,
                       -578334559/42, -699043/133, -1/420]

upperCoefficients12 :: [Double]
upperCoefficients12 = [1/12, 1/24, 19/360, 863/2520, 1375/1008,
                       33953/5040, 57281/1440, 3250433/11880,
                       378351/176, -37521922090657/720720,
                       -612415657466657/131040, -3476857538815223/65520,
                       -243882174660761/1440, -34160796727900637/183600,
                       -39453820646687/544, -750984629069237/81396,
                       -2934056300989/9576, -20394527513/12540,
                       -3829559/9240, -1/10626]

lowerCoefficients12 :: [Double]
lowerCoefficients12 = [0,0,0,0,0,0,0,0,0,0,0,
                       -105411381075/4, -5270569053750, -272908057767345/2,
                       -1051953238104769, -24557168490009155/8,
                       -3683261873403112, -5461918738302026/3,
                       -347362037754732, -2205885452434521/100,
                       -12237195698286/35, -16926981721/22,
                       -6710881/155, -1/600]

-- | Compute entropy directly from its definition. This is just as accurate
-- as 'alyThm1' for lambda <= 1 and is faster, but is slow for large lambda,
-- and produces some underestimation due to accumulation of floating point
-- error.
directEntropy :: Double -> Double
directEntropy lambda =
  negate . sum $
  takeWhile (< negate m_epsilon * lambda) $
  dropWhile (not . (< negate m_epsilon * lambda)) $
  [ let x = probability lambda k in x * log x | k <- [0..]]

-- | Compute the entropy of a Poisson distribution using the best available
-- method.
poissonEntropy :: Double -> Double
poissonEntropy lambda
  | lambda == 0 = 0
  | lambda <= 10 = directEntropy lambda
  | lambda <= 12 = alyThm2 lambda upperCoefficients4 lowerCoefficients4
  | lambda <= 18 = alyThm2 lambda upperCoefficients6 lowerCoefficients6
  | lambda <= 24 = alyThm2 lambda upperCoefficients8 lowerCoefficients8
  | lambda <= 30 = alyThm2 lambda upperCoefficients10 lowerCoefficients10
  | otherwise = alyThm2 lambda upperCoefficients12 lowerCoefficients12
