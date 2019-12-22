{-# LANGUAGE ViewPatterns #-}
-- |
-- Tests for quantile
module Tests.Quantile (tests) where

import Control.Exception
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (sample)
import Numeric.MathFunctions.Comparison (ulpDelta,ulpDistance)
import Statistics.Quantile

tests :: TestTree
tests = testGroup "Quantiles"
  [ testCase "R alg. 4" $ compareWithR cadpw (0.00, 0.50, 2.50, 8.25, 10.00)
  , testCase "R alg. 5" $ compareWithR hazen (0.00, 1.00, 5.00, 9.00, 10.00)
  , testCase "R alg. 6" $ compareWithR spss  (0.00, 0.75, 5.00, 9.25, 10.00)
  , testCase "R alg. 7" $ compareWithR s     (0.000, 1.375, 5.000, 8.625,10.00)
  , testCase "R alg. 8" $ compareWithR medianUnbiased
      (0.0, 0.9166666666666667, 5.000000000000003, 9.083333333333334, 10.0)
  , testCase "R alg. 9" $ compareWithR normalUnbiased
      (0.0000, 0.9375, 5.0000, 9.0625, 10.0000)
  , testProperty "alg 7." propWeigtedAverage
    -- Test failures
  , testCase "weightedAvg should throw errors" $ do
      let xs  = U.fromList [1,2,3]
          xs0 = U.fromList []
      shouldError "Empty sample" $ weightedAvg 1 4 xs0
      shouldError "N=0"  $ weightedAvg 1 0 xs
      shouldError "N=1"  $ weightedAvg 1 1 xs
      shouldError "k<0"  $ weightedAvg (-1) 4 xs
      shouldError "k>N"  $ weightedAvg 5    4 xs
  , testCase "quantile should throw errors" $ do
      let xs  = U.fromList [1,2,3]
          xs0 = U.fromList []
      shouldError "Empty xs" $ quantile s 1 4 xs0
      shouldError "N=0"  $ quantile s 1 0 xs
      shouldError "N=1"  $ quantile s 1 1 xs
      shouldError "k<0"  $ quantile s (-1) 4 xs
      shouldError "k>N"  $ quantile s 5    4 xs
    --
  , testProperty "quantiles    are OK" propQuantiles
  , testProperty "quantilesVec are OK" propQuantilesVec
  ]

sample :: U.Vector Double
sample = U.fromList [0, 1, 2.5, 7.5, 9, 10]

-- Compare quantiles implementation with reference R implementation
compareWithR :: ContParam -> (Double,Double,Double,Double,Double) -> Assertion
compareWithR p (q0,q1,q2,q3,q4) = do
  assertEqual "Q 0" q0 $ quantile p 0 4 sample
  assertEqual "Q 1" q1 $ quantile p 1 4 sample
  assertEqual "Q 2" q2 $ quantile p 2 4 sample
  assertEqual "Q 3" q3 $ quantile p 3 4 sample
  assertEqual "Q 4" q4 $ quantile p 4 4 sample

propWeigtedAverage :: Positive Int -> Positive Int -> Property
propWeigtedAverage (Positive k) (Positive q) =
  (q >= 2 && k <= q) ==> let q1 = weightedAvg k q sample
                             q2 = quantile s k q sample
                         in counterexample ("weightedAvg   = " ++ show q1)
                          $ counterexample ("quantile      = " ++ show q2)
                          $ counterexample ("delta in ulps = " ++ show (ulpDelta q1 q2))
                          $ ulpDistance q1 q2 <= 16

propQuantiles :: Positive Int -> Int -> Int -> NonEmptyList Double -> Property
propQuantiles (Positive n)
              ((`mod` n) -> k1)
              ((`mod` n) -> k2)
              (NonEmpty xs)
  =   n >= 2
  ==> [x1,x2] == quantiles s [k1,k2] n rndXs
  where
    rndXs = U.fromList xs
    x1 = quantile s k1 n rndXs
    x2 = quantile s k2 n rndXs

propQuantilesVec :: Positive Int -> Int -> Int -> NonEmptyList Double -> Property
propQuantilesVec (Positive n)
                 ((`mod` n) -> k1)
                 ((`mod` n) -> k2)
                 (NonEmpty xs)
  =   n >= 2
  ==> U.fromList [x1,x2] == quantilesVec s (U.fromList [k1,k2]) n rndXs
  where
    rndXs = U.fromList xs
    x1 = quantile s k1 n rndXs
    x2 = quantile s k2 n rndXs


shouldError :: String -> a -> Assertion
shouldError nm x = do
  r <- try (evaluate x)
  case r of
    Left  (ErrorCall{}) -> return ()
    Right _             -> assertFailure ("Should call error: " ++ nm)
