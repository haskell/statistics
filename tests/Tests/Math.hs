-- | Tests for Statistics.Math
module Tests.Math (
  mathTests
  ) where

import Data.Vector.Unboxed (fromList)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Tests.Helpers
import Statistics.Math


mathTests :: Test
mathTests = testGroup "S.Math"
  [ testProperty "Γ(x+1) = x·Γ(x) logGamma"  $ gammaReccurence logGamma  3e-8
  , testProperty "Γ(x+1) = x·Γ(x) logGammaL" $ gammaReccurence logGammaL 2e-13
  , testProperty "γ(1,x) = 1 - exp(-x)"      $ incompleteGammaAt1Check
  , testProperty "γ - increases"             $
      \s x y -> s > 0 && x > 0 && y > 0 ==> monotonicallyIncreases (incompleteGamma s) x y
  , chebyshevTests
  ]

----------------------------------------------------------------
--
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