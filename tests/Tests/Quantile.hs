-- |
-- Tests for quantile
module Tests.Quantile (tests) where

import qualified Data.Vector.Unboxed as U
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (Assertion,assertEqual)
import Test.QuickCheck hiding (sample)
import Numeric.MathFunctions.Comparison (ulpDelta,ulpDistance)
import Statistics.Quantile

tests :: Test
tests = testGroup "Quantiles"
  [ testCase "R alg. 4" $ compareWithR cadpw (0.00, 0.50, 2.50, 8.25, 10.00)
  , testCase "R alg. 5" $ compareWithR hazen (0.00, 1.00, 5.00, 9.00, 10.00)
  , testCase "R alg. 6" $ compareWithR spss  (0.00, 0.75, 5.00, 9.25, 10.00)
  , testCase "R alg. 7" $ compareWithR s     (0.000, 1.375, 5.000, 8.625,10.00)
  , testCase "R alg. 8" $ compareWithR medianUnbiased
      (0.0, 0.9166666666666667, 5.000000000000003, 9.083333333333334, 10.0)
  , testCase "R alg. 9" $ compareWithR normalUnbiased
      (0.0000, 0.9375, 5.0000, 9.0625, 10.0000)
  , testProperty "alg 7." testWeigtedAverage
  ]

sample :: U.Vector Double
sample = U.fromList [0, 1, 2.5, 7.5, 9, 10]

-- Compare quantiles implementation with reference R implementation
compareWithR :: ContParam -> (Double,Double,Double,Double,Double) -> Assertion
compareWithR p (q0,q1,q2,q3,q4) = do
  assertEqual "Q 0" q0 $ continuousBy p 0 4 sample
  assertEqual "Q 1" q1 $ continuousBy p 1 4 sample
  assertEqual "Q 2" q2 $ continuousBy p 2 4 sample
  assertEqual "Q 3" q3 $ continuousBy p 3 4 sample
  assertEqual "Q 4" q4 $ continuousBy p 4 4 sample

testWeigtedAverage :: Positive Int -> Positive Int -> Property
testWeigtedAverage (Positive k) (Positive q) =
  (q >= 2 && k <= q) ==> let q1 = weightedAvg k q sample
                             q2 = continuousBy s k q sample
                         in counterexample ("weightedAvg   = " ++ show q1)
                          $ counterexample ("continuousBy  = " ++ show q2)
                          $ counterexample ("delta in ulps = " ++ show (ulpDelta q1 q2))
                          $ ulpDistance q1 q2 <= 16
