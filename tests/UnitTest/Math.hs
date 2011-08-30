-- Tests for Statistics.Math
module UnitTest.Math ( 
    mathTests
  ) where

import qualified Data.Vector as V
import           Data.Vector   ((!))
import Test.HUnit

import Statistics.Math


mathTests :: [Test] 
mathTests = [ TestCase $ assertBool "Factorial is expected to be precise at 1e-15 level" $
                all (< 1e-15) $ map factorialErr [0..170]
            , TestCase $ assertBool "Log factorial is expected to be precise at 1e-15 level" $
                all (< 1e-15) $ map factorialLogErr [2..170]
            , TestCase $ assertBool "logGamma is expected to be precise at 1e-9 level" $
                all (< 1e-9)  $ map logGammaErr [3..10000]
            , TestCase $ assertBool "logGammaL is expected to be precise at 1e-15 level" $
                all (< 1e-15) $ map logGammaLErr [3..10000]
            , TestCase $ assertBool "logBeta is expected to be precise at 3e-8 level" $
                all (< 3e-8)  [logBetaErr a b | a <- [0.1,0.2 .. 100], b <- [0.1,0.2 .. 100]]
            , TestCase $ assertBool "choose is expected to precise at 1e-12 level" $
                all (< 1e-12) [chooseErr  n k | n <- [0..1000], k <- [0..n]]
            ]

----------------------------------------------------------------

-- Lookup table for fact factorial calculation. It has fixed size
-- which is bad but it's OK for this particular case
factorial_table :: V.Vector Integer
factorial_table = V.generate 2000 (\n -> product [1..fromIntegral n])

-- Exact implementation of factorial
factorial' :: Integer -> Integer
factorial' n = factorial_table ! fromIntegral n

-- Exact albeit slow implementation of choose
choose' :: Integer -> Integer -> Integer
choose' n k = factorial' n `div` (factorial' k * factorial' (n-k))

-- Error in determination of factorial
factorialErr :: Integer -> Double
factorialErr n = (f' - f) / f'
  where
    f' = fromIntegral (factorial' n)
    f  = factorial (fromIntegral n)

-- Error in determination of log of factorial
factorialLogErr :: Integer -> Double
factorialLogErr n = (f' - f) / f'
  where
    f' = log $ fromIntegral $ factorial' n
    f  = logFactorial (fromIntegral n)

-- Error in determination if binmial coef.
chooseErr :: Integer -> Integer -> Double 
chooseErr n k = (c - c') / c'
  where
    c' = fromIntegral (choose' n k)
    c  = choose (fromIntegral n) (fromIntegral k)

-- Error in logGamma function for integer points > 2
logGammaErr :: Int -> Double
logGammaErr n = (logGamma (fromIntegral n) - l) / l where l = logFactorial n

-- Error in logGammaL function for integer points > 2
logGammaLErr :: Int -> Double
logGammaLErr n = (logGammaL (fromIntegral n) - l) / l where l = logFactorial n

-- Test beta function.
-- FIXME: I'm not sure whether it's correct test.
logBetaErr :: Double -> Double -> Double
logBetaErr p q = (lb' - lb) / max 1 (abs lb')
  where
    lb  = logBeta p q
    lb' = logGammaL p + logGammaL q - logGammaL (p+q)
