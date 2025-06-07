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
tests = testGroup "Parametric tests" [studentTTests, bartlettTests, leveneTests]

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

studentTTests :: Tst.TestTree
studentTTests = testGroup "StudentT test" $ concat
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

bartlettTests :: TestTree
bartlettTests = testGroup "Bartlett's test"
  [ testCase "a,b,c" $ testBartlettTest [a,b,c] 1.8027132567760222   0.40601846976301237
  , testCase "a,b"   $ testBartlettTest [a,b]   0.005221063776321886 0.9423974408021293
  , testCase "a,c"   $ testBartlettTest [a,c]   1.1531619271845452   0.2828882244527482
  , testCase "a,a"   $ testBartlettTest [a,a]   0.0                  1.0
  ]
  where
    a = U.fromList [9.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
    b = U.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 9.36, 9.18, 8.67, 9.05]
    c = U.fromList [8.95, 8.12, 8.95, 8.85, 8.03, 8.84, 8.07, 8.98, 8.86, 8.98]

testBartlettTest
  :: [U.Vector Double]
  -> Double
  -> Double
  -> IO ()
testBartlettTest samples w p = do
  r <- case bartlettTest samples of
    Left  _ -> error "Bartlett's test failed"
    Right r -> pure r
  approxEqual "W" 1e-9 (testStatistics r)            w
  approxEqual "p" 1e-9 (pValue $ testSignificance r) p

------------------------------------------------------------
-- Levene's Test (Trimmed Mean)
------------------------------------------------------------

leveneTests :: TestTree
leveneTests = testGroup "Levene test"
  -- Statistics' value and p-values are computed using 
  [ testCase "a,b,c Mean"    $ testLeveneTest [a,b,c] Mean   7.905194483442054 0.001983795817472731
  , testCase "a,b   Mean"    $ testLeveneTest [a,b]   Mean   8.83873787256358  0.008149720958328811
  , testCase "a,a   Mean"    $ testLeveneTest [a,a]   Mean   0.0               1.0
  , testCase "a,b,c Median"  $ testLeveneTest [a,b,c] Median 7.584952754501659 0.002431505967249681
  , testCase "a,b   Median"  $ testLeveneTest [a,b]   Median 8.461374333228711 0.009364737715584399
  , testCase "aL,bL Mean"    $ testLeveneTest [aL,bL] Mean   5.84424549939465  0.01653410652558999
  , testCase "aL,bL Trimmed" $ testLeveneTest [aL,bL] (Trimmed 0.05) 8.368311226366314 0.004294953946529551
  ]
  where
    a = V.fromList [8.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
    b = V.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 8.36, 9.18, 8.67, 9.05]
    c = V.fromList [8.95, 9.12, 8.95, 8.85, 9.03, 8.84, 9.07, 8.98, 8.86, 8.98]
    -- Large samples for testing trimmed
    aL = V.fromList [
      -0.18919252, -1.62837673,  5.21332355, -0.00962043, -0.28417847,
      -0.88128233,  1.49698436,  6.1780359 , -1.22301348,  3.34598245,
       5.33227264, -0.88732069,  0.14487346,  2.61060215,  4.22033907,
       2.53139215, -0.72131061,  0.53063607, -0.60510374, -0.73230842,
       1.54037043, -2.81103963,  3.40763063,  0.49005324,  2.13085513,
       5.68650547,  4.16397279, -0.17325097,  1.12664972,  4.23297516,
       4.15943436, -1.01452078,  2.40391646,  0.83019962,  0.29665879,
      -3.83031046, -1.98576933,  1.5356527 ,  1.30773365,  0.292818  ,
       2.45877828,  1.06482289, -0.63241873,  1.58465379,  1.96577614,
       2.25791943,  4.13769848, -2.38595767, -0.65801423, -2.54007791,
       3.17428087,  4.32096964,  0.92240335, -2.38101319,  1.35692587,
       1.48279101, -0.04438309,  0.50296642,  2.08261495,  1.33181215,
      -1.95427198,  4.95406809,  1.51294898, -2.68536129, -0.2441218 ,
       2.41142613,  4.71051493,  2.66618697,  1.12668301, -0.25732583,
       1.25021838, -1.27523641,  5.01638744,  3.38864442,  0.17979744,
      -0.88481645,  3.89346357, -0.51512217, -1.60542888,  0.88378679,
      -2.12962732, -1.35989539,  5.09215112, -1.37442481,  0.83578405,
       0.13829571,  1.25171481,  3.60552158, -3.24051591, -0.44301834,
       0.78253445,  1.76098254,  1.79677434, -0.19010505,  3.07640466,
       3.02853882,  1.24849063,  4.84505382,  6.82274999,  2.24063474]
    bL = V.fromList [
        2.15584101, -2.74876744, -0.82231894,  1.97518087,  2.59280595,
        1.28703417,  2.40450278,  1.9761031 ,  2.35186598,  1.15611047,
        2.26709318,  1.2832138 , -2.1486074 ,  0.27563011, -0.51816861,
        0.89658424,  3.27069545,  1.72846646,  3.84454277,  5.58301459,
       -0.40878188,  3.41602853,  1.1281526 ,  0.9665913 ,  0.76567084,
        1.69522855,  1.69133014,  0.70529264,  2.65243202, -1.0088019 ,
       -0.62431026,  3.76667396,  3.66225181,  0.73217579,  0.04478736,
        0.4169833 ,  0.77065631, -1.31484093,  1.23858618, -0.08339456,
        3.14154286,  1.84358218, -0.53511423, -3.4919477 ,  0.24076997,
        3.59381684,  1.99497806,  2.95499775,  1.67157731,  0.0214764 ,
        3.32161612, -2.64762427,  0.06486472,  0.19653897,  1.34954235,
        1.18568747, -0.54434597, -3.35544223,  1.41933109,  0.95100195,
        2.7182116 ,  1.1334068 , -0.95297806, -0.05421818,  1.42248799,
       -3.96201277, -3.21309254, -0.21209211,  0.9689551 ,  0.13526401,
       -0.88656198,  0.41331783, -3.18766064,  4.34948246,  1.35656384,
        0.41920101, -0.46578994,  1.55181583,  2.43937014,  2.49040644,
        4.10505494,  1.68856296,  1.31503895,  0.41123368,  0.73242999,
        0.2804349 , -1.83494592, -0.31073195,  2.61185513,  2.91645094,
        1.26097638,  2.64197134,  3.88931972,  0.03783002,  2.55209729,
        3.46869549,  0.96348003,  2.27658242,  2.7613171 , -0.1372434 ]

    
testLeveneTest
  :: [V.Vector Double]
  -> Center
  -> Double
  -> Double
  -> IO ()
testLeveneTest samples center w p = do
  r <- case levenesTest center samples of
    Left  _ -> error "Levene's test failed"
    Right r -> pure r
  approxEqual "W" 1e-9 (testStatistics r)            w
  approxEqual "p" 1e-9 (pValue $ testSignificance r) p


----------------------------------------------------------------

approxEqual :: String -> Double -> Double -> Double -> IO ()
approxEqual name epsilon actual expected =
  assertBool (name ++ ": expected ≈ " ++ show expected ++ ", got " ++ show actual)
             (diff < epsilon)
  where
    diff = abs (actual - expected)
