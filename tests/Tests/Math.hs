{-# LANGUAGE ViewPatterns #-}
-- | Tests for Statistics.Math
module Tests.Math (
  mathTests
  ) where

import Data.Vector.Unboxed (fromList)
import qualified Data.Vector as V
import           Data.Vector   ((!))

import Test.QuickCheck  hiding (choose)
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Tests.Helpers
import Tests.Math.Tables
import Statistics.Math


mathTests :: Test
mathTests = testGroup "S.Math"
  [ testProperty "Γ(x+1) = x·Γ(x) logGamma"  $ gammaReccurence logGamma  3e-8
  , testProperty "Γ(x+1) = x·Γ(x) logGammaL" $ gammaReccurence logGammaL 2e-13
  , testProperty "γ(1,x) = 1 - exp(-x)"      $ incompleteGammaAt1Check
  , testProperty "γ - increases"             $
      \s x y -> s > 0 && x > 0 && y > 0 ==> monotonicallyIncreases (incompleteGamma s) x y
  , testProperty "invIncompleteGamma = γ^-1" $ invIGammaIsInverse
  , chebyshevTests
    -- Unit tests
  , testAssertion "Factorial is expected to be precise at 1e-15 level"
      $ and [ eq 1e-15 (factorial (fromIntegral n))
                       (fromIntegral (factorial' n))
            |n <- [0..170]]
  , testAssertion "Log factorial is expected to be precise at 1e-15 level"
      $ and [ eq 1e-15 (logFactorial (fromIntegral n))
                       (log $ fromIntegral $ factorial' n)
            | n <- [2..170]]
  , testAssertion "logGamma is expected to be precise at 1e-9 level [integer points]"
      $ and [ eq 1e-9 (logGamma (fromIntegral n))
                      (logFactorial (n-1))
            | n <- [3..10000]]
  , testAssertion "logGamma is expected to be precise at 1e-9 level [fractional points]"
      $ and [ eq 1e-9 (logGamma x) lg | (x,lg) <- tableLogGamma ]
  , testAssertion "logGammaL is expected to be precise at 1e-15 level"
      $ and [ eq 1e-15 (logGammaL (fromIntegral n))
                       (logFactorial (n-1))
            | n <- [3..10000]]
  , testAssertion "logGammaL is expected to be precise at 1e-9 level [fractional points]"
      $ and [ eq 1e-10 (logGammaL x) lg | (x,lg) <- tableLogGamma ]
  , testAssertion "logBeta is expected to be precise at 1e-6 level"
      $ and [ eq 1e-6 (logBeta p q)
                      (logGammaL p + logGammaL q - logGammaL (p+q))
            | p <- [0.1,0.2 .. 0.9] ++ [2 .. 20]
            , q <- [0.1,0.2 .. 0.9] ++ [2 .. 20]]
  -- FIXME: Why 1e-8? Is it due to poor precision of logBeta?
  , testAssertion "incompleteBeta is expected to be precise at 1e-8 level"
      $ and [ eq 1e-8 (incompleteBeta p q x) ib | (p,q,x,ib) <- tableIncompleteBeta ]
  , testAssertion "choose is expected to precise at 1e-12 level"
      $ and [ eq 1e-12 (choose (fromIntegral n) (fromIntegral k)) (fromIntegral $ choose' n k)
            | n <- [0..300], k <- [0..n]]
  ]

----------------------------------------------------------------
-- QC tests
----------------------------------------------------------------

-- Γ(x+1) = x·Γ(x)
gammaReccurence :: (Double -> Double) -> Double -> Double -> Property
gammaReccurence logG ε x =
  (x > 0 && x < 100)  ==>  (abs (g2 - g1 - log x) < ε)
    where
      g1 = logG x
      g2 = logG (x+1)


-- γ(1,x) = 1 - exp(-x)
-- Since Γ(1) = 1 normalization doesn't make any difference
incompleteGammaAt1Check :: Double -> Property
incompleteGammaAt1Check x =
  x > 0 ==> (incompleteGamma 1 x + exp(-x)) ≈ 1
  where
    (≈) = eq 1e-13

-- invIncompleteGamma is inverse of incompleteGamma
invIGammaIsInverse :: Double -> Double -> Property
invIGammaIsInverse (abs -> a) (abs . snd . properFraction -> p) =
  a > 0 && p > 0 && p < 1  ==> ( printTestCase ("x  = " ++ show x )
                               $ printTestCase ("p' = " ++ show p')
                               $ printTestCase ("Δp = " ++ show (p - p'))
                               $ abs (p - p') <= 1e-12
                               )
  where
    x  = invIncompleteGamma a p
    p' = incompleteGamma    a x


-- Test that Chebyshev polynomial of low order are evaluated correctly
chebyshevTests :: Test
chebyshevTests = testGroup "Chebyshev polynomials"
  [ testProperty "Chebyshev 0" $ \a0 (Ch x) ->
      (ch0 x * a0) ≈ (chebyshev x $ fromList [a0])
  , testProperty "Chebyshev 1" $ \a0 a1 (Ch x) ->
      (a0*ch0 x + a1*ch1 x) ≈  (chebyshev x $ fromList [a0,a1])
  , testProperty "Chebyshev 2" $ \a0 a1 a2 (Ch x) ->
       (a0*ch0 x + a1*ch1 x + a2*ch2 x) ≈ (chebyshev x $ fromList [a0,a1,a2])
  , testProperty "Chebyshev 3" $ \a0 a1 a2 a3 (Ch x) ->
       (a0*ch0 x + a1*ch1 x + a2*ch2 x + a3*ch3 x) ≈ (chebyshev x $ fromList [a0,a1,a2,a3])
  , testProperty "Chebyshev 4" $ \a0 a1 a2 a3 a4 (Ch x) ->
       (a0*ch0 x + a1*ch1 x + a2*ch2 x + a3*ch3 x + a4*ch4 x) ≈ (chebyshev x $ fromList [a0,a1,a2,a3,a4])
  ]
  where (≈) = eq 1e-12

-- Chebyshev polynomials of low order
ch0,ch1,ch2,ch3,ch4 :: Double -> Double
ch0 _ = 1
ch1 x = x
ch2 x = 2*x^2 - 1
ch3 x = 4*x^3 - 3*x
ch4 x = 8*x^4 - 8*x^2 + 1

newtype Ch = Ch Double
             deriving Show
instance Arbitrary Ch  where
  arbitrary = do x <- arbitrary
                 return $ Ch $ 2 * (snd . properFraction) x - 1



----------------------------------------------------------------
-- Unit tests
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
