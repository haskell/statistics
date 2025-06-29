module Tests.Parametric (tests) where

import Data.Maybe (fromJust)
import Statistics.Test.StudentT
import Statistics.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertBool)
import Tests.Helpers (testEquality)
import qualified Test.Tasty as Tst

import Statistics.Test.Levene
import Statistics.Test.Bartlett


tests :: Tst.TestTree
tests = testGroup "Parametric tests" (studentTTests ++ bartlettTests ++ leveneTests)

-- 2 samples x 20 obs data
--
-- Both samples are samples from normal distributions with the same variance (= 1.0),
-- but their means are different (0.0 and 0.5, respectively).
--
-- You can reproduce the data with R (3.1.0) as follows:
--   set.seed(0)
--   sample1 = rnorm(20)
--   sample2 = rnorm(20, 0.5)
--   student = t.test(sample1, sample2, var.equal=T)
--   welch = t.test(sample1, sample2)
--   paired = t.test(sample1, sample2, paired=T)
sample1, sample2 :: U.Vector Double
sample1 = U.fromList [
  1.262954284880793e+00,
 -3.262333607056494e-01,
  1.329799262922501e+00,
  1.272429321429405e+00,
  4.146414344564082e-01,
 -1.539950041903710e+00,
 -9.285670347135381e-01,
 -2.947204467905602e-01,
 -5.767172747536955e-03,
  2.404653388857951e+00,
  7.635934611404596e-01,
 -7.990092489893682e-01,
 -1.147657009236351e+00,
 -2.894615736882233e-01,
 -2.992151178973161e-01,
 -4.115108327950670e-01,
  2.522234481561323e-01,
 -8.919211272845686e-01,
  4.356832993557186e-01,
 -1.237538421929958e+00]
sample2 = U.fromList [
  2.757321147216907e-01,
  8.773956459817011e-01,
  6.333363608148415e-01,
  1.304189509744908e+00,
  4.428932256161913e-01,
  1.003607972233726e+00,
  1.585769362145687e+00,
 -1.909538396968303e-01,
 -7.845993538721883e-01,
  5.467261721883520e-01,
  2.642934435604988e-01,
 -4.288825501025439e-02,
  6.668968254321778e-02,
 -1.494716467962331e-01,
  1.226750747385451e+00,
  1.651911754087200e+00,
  1.492160365445798e+00,
  7.048689050811874e-02,
  1.738304100853380e+00,
  2.206537181457307e-01]


testTTest :: String
          -> PValue Double
          -> Test d
          -> [Tst.TestTree]
testTTest name pVal test =
  [ testEquality name (isSignificant pVal test) NotSignificant
  , testEquality name (isSignificant (mkPValue $ pValue pVal + 1e-5) test)
    Significant
  ]

studentTTests :: [Tst.TestTree]
studentTTests = concat
  [ -- R: t.test(sample1, sample2, alt="two.sided", var.equal=T)
    testTTest "two-sample t-test SamplesDiffer Student"
      (mkPValue 0.03410) (fromJust $ studentTTest SamplesDiffer sample1 sample2)
    -- R: t.test(sample1, sample2, alt="two.sided", var.equal=F)
  , testTTest "two-sample t-test SamplesDiffer Welch"
      (mkPValue 0.03483) (fromJust $ welchTTest SamplesDiffer sample1 sample2)
    -- R: t.test(sample1, sample2, alt="two.sided", paired=T)
  , testTTest "two-sample t-test SamplesDiffer Paired"
      (mkPValue 0.03411) (fromJust $ pairedTTest SamplesDiffer sample12)
    -- R: t.test(sample1, sample2, alt="less", var.equal=T)
  , testTTest "two-sample t-test BGreater Student"
      (mkPValue 0.01705) (fromJust $ studentTTest BGreater sample1 sample2)
    -- R: t.test(sample1, sample2, alt="less", var.equal=F)
  , testTTest "two-sample t-test BGreater Welch"
      (mkPValue 0.01741) (fromJust $ welchTTest BGreater sample1 sample2)
    -- R: t.test(sample1, sample2, alt="less", paired=F)
  , testTTest "two-sample t-test BGreater Paired"
      (mkPValue 0.01705) (fromJust $ pairedTTest BGreater sample12)
  ]
  where sample12 = U.zip sample1 sample2


------------------------------------------------------------
-- Bartlett's Test
------------------------------------------------------------

bartlettTests :: [TestTree]
bartlettTests =
  let
    a = U.fromList [9.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
    b = U.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 9.36, 9.18, 8.67, 9.05]
    c = U.fromList [8.95, 8.12, 8.95, 8.85, 8.03, 8.84, 8.07, 8.98, 8.86, 8.98]
    result = bartlettTest [a, b, c]
  in case result of
       Left err -> [testCase "Bartlett test - failed" (assertBool err False)]
       Right test ->
         [ testApproxEqual "Bartlett's Chi-Square Statistic" 1e-3 (testStatistics test) 1.802
         , testApproxEqual "Bartlett's P-Value" 1e-3 (pValue $ testSignificance test) 0.406
         , testEquality "Reject null hypothesis at alpha = 0.05"
             (isSignificant (mkPValue 0.05) test) NotSignificant
         ]

------------------------------------------------------------
-- Levene's Test (Trimmed Mean)
------------------------------------------------------------

-- Local helper for approximate equality
testApproxEqual :: String -> Double -> Double -> Double -> TestTree
testApproxEqual name epsilon actual expected =
  testCase name $
    let diff = abs (actual - expected)
    in assertBool (name ++ ": expected ≈ " ++ show expected ++ ", got " ++ show actual) (diff < epsilon)

leveneTests :: [TestTree]
leveneTests =
  let
    a = V.fromList [8.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
    b = V.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 8.36, 9.18, 8.67, 9.05]
    c = V.fromList [8.95, 9.12, 8.95, 8.85, 9.03, 8.84, 9.07, 8.98, 8.86, 8.98]
    result = levenesTest 0.05 (Trimmed 0.05) [a, b, c]
  in case result of
       Left err -> [testCase "Levene trimmed mean test - failed" (assertBool err False)]
       Right test ->
         [ testApproxEqual "Levene's W Statistic (Trimmed 0.05)" 1e-3 (testStatistics test) 7.905
         , testApproxEqual "Levene's P-Value (Trimmed 0.05)" 1e-3 (pValue $ testSignificance test) 0.002
         , testEquality "Reject null hypothesis at alpha = 0.05"
             (isSignificant (mkPValue 0.05) test) Significant
         ]
