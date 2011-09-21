-- Tests for Statistics.Test.NonParametric
module Tests.NonparametricTest (
  nonparametricTests
  ) where


import qualified Data.Vector.Unboxed as U
import Test.HUnit                     (Test(..),assertEqual,assertBool)
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit

import Statistics.Test.NonParametric




nonparametricTests :: TF.Test
nonparametricTests = TF.testGroup "Nonparametric tests"
                   $ hUnitTestToTests =<< concat [ mannWhitneyTests
                                                 , wilcoxonSumTests
                                                 , wilcoxonPairTests
                                                 ]


----------------------------------------------------------------

mannWhitneyTests :: [Test]
mannWhitneyTests = zipWith test [(0::Int)..] testData ++
  [TestCase $ assertEqual "Mann-Whitney U Critical Values, m=1"
    (replicate (20*3) Nothing)
    [mannWhitneyUCriticalValue (1,x) p | x <- [1..20], p <- [0.005,0.01,0.025]]
  ,TestCase $ assertEqual "Mann-Whitney U Critical Values, m=2, p=0.025"
    (replicate 7 Nothing ++ map Just [0,0,0,0,1,1,1,1,1,2,2,2,2])
    [mannWhitneyUCriticalValue (2,x) 0.025 | x <- [1..20]]
  ,TestCase $ assertEqual "Mann-Whitney U Critical Values, m=6, p=0.05"
    (replicate 1 Nothing ++ map Just [0, 2,3,5,7,8,10,12,14,16,17,19,21,23,25,26,28,30,32])
    [mannWhitneyUCriticalValue (6,x) 0.05 | x <- [1..20]]
  ,TestCase $ assertEqual "Mann-Whitney U Critical Values, m=20, p=0.025"
    (replicate 1 Nothing ++ map Just [2,8,14,20,27,34,41,48,55,62,69,76,83,90,98,105,112,119,127])
    [mannWhitneyUCriticalValue (20,x) 0.025 | x <- [1..20]]
  ]
  where
    test n (a, b, c, d)
      = TestCase $ do assertEqual ("Mann-Whitney U "     ++ show n) c us
                      assertEqual ("Mann-Whitney U Sig " ++ show n)
                        d $ mannWhitneyUSignificant TwoTailed (length a, length b) 0.05 us
      where
        us = mannWhitneyU (U.fromList a) (U.fromList b)

    -- List of (Sample A, Sample B, (Positive Rank, Negative Rank))
    testData :: [([Double], [Double], (Double, Double), Maybe Bool)]
    testData = [ ( [3,4,2,6,2,5]
                 , [9,7,5,10,6,8]
                 , (2, 34)
                 , Just True
                 )
               , ( [540,480,600,590,605]
                 , [760,890,1105,595,940]
                 , (2, 23)
                 , Just True
                 )
               , ( [19,22,16,29,24]
                 , [20,11,17,12]
                 , (17, 3)
                 , Just False
                 )
               , ( [126,148,85,61, 179,93, 45,189,85,93]
                 , [194,128,69,135,171,149,89,248,79,137]
                 , (35,65)
                 , Just False
                 )
               , ( [1..30]
                 , [1..30]
                 , (450,450)
                 , Just False
                 )
               , ( [1 .. 30]
                 , [11.5 .. 40 ]
                 , (190.0,710.0)
                 , Just True
                 )
               ]

wilcoxonSumTests :: [Test]
wilcoxonSumTests = zipWith test [(0::Int)..] testData
  where
    test n (a, b, c) = TestCase $ assertEqual ("Wilcoxon Sum " ++ show n) c (wilcoxonRankSums (U.fromList a) (U.fromList b))

    -- List of (Sample A, Sample B, (Positive Rank, Negative Rank))
    testData :: [([Double], [Double], (Double, Double))]
    testData = [ ( [8.50,9.48,8.65,8.16,8.83,7.76,8.63]
                 , [8.27,8.20,8.25,8.14,9.00,8.10,7.20,8.32,7.70]
                 , (75, 61)
                 )
               , ( [0.45,0.50,0.61,0.63,0.75,0.85,0.93]
                 , [0.44,0.45,0.52,0.53,0.56,0.58,0.58,0.65,0.79]
                 , (71.5, 64.5)
                 )
               ]

wilcoxonPairTests :: [Test]
wilcoxonPairTests = zipWith test [(0::Int)..] testData ++
  -- Taken from the Mitic paper:
  [ TestCase $ assertBool "Sig 16, 35" (to4dp 0.0467 $ wilcoxonMatchedPairSignificance 16 35)
  , TestCase $ assertBool "Sig 16, 36" (to4dp 0.0523 $ wilcoxonMatchedPairSignificance 16 36)
  , TestCase $ assertEqual "Wilcoxon critical values, p=0.05"
      (replicate 4 Nothing ++ map Just [0,2,3,5,8,10,13,17,21,25,30,35,41,47,53,60,67,75,83,91,100,110,119])
      [wilcoxonMatchedPairCriticalValue x 0.05 | x <- [1..27]]
  , TestCase $ assertEqual "Wilcoxon critical values, p=0.025"
      (replicate 5 Nothing ++ map Just [0,2,3,5,8,10,13,17,21,25,29,34,40,46,52,58,65,73,81,89,98,107])
      [wilcoxonMatchedPairCriticalValue x 0.025 | x <- [1..27]]
  , TestCase $ assertEqual "Wilcoxon critical values, p=0.01"
      (replicate 6 Nothing ++ map Just [0,1,3,5,7,9,12,15,19,23,27,32,37,43,49,55,62,69,76,84,92])
      [wilcoxonMatchedPairCriticalValue x 0.01 | x <- [1..27]]
  , TestCase $ assertEqual "Wilcoxon critical values, p=0.005"
      (replicate 7 Nothing ++ map Just [0,1,3,5,7,9,12,15,19,23,27,32,37,42,48,54,61,68,75,83])
      [wilcoxonMatchedPairCriticalValue x 0.005 | x <- [1..27]]
  ]
  where
    test n (a, b, c) = TestCase $ assertEqual ("Wilcoxon Paired " ++ show n) c (wilcoxonMatchedPairSignedRank (U.fromList a) (U.fromList b))

    -- List of (Sample A, Sample B, (Positive Rank, Negative Rank))
    testData :: [([Double], [Double], (Double, Double))]
    testData = [ ([1..10], [1..10], (0, 0     ))
               , ([1..5],  [6..10], (0, 5*(-3)))
               -- Worked example from the Internet:
               , ( [125,115,130,140,140,115,140,125,140,135]
                 , [110,122,125,120,140,124,123,137,135,145]
                 , ( sum $ filter (> 0) [7,-3,1.5,9,0,-4,8,-6,1.5,-5]
                   , sum $ filter (< 0) [7,-3,1.5,9,0,-4,8,-6,1.5,-5]
                   )
                 )
               -- Worked examples from books/papers:
               , ( [2.4,1.9,2.3,1.9,2.4,2.5]
                 , [2.0,2.1,2.0,2.0,1.8,2.0]
                 , (18, -3)
                 )
               , ( [130,170,125,170,130,130,145,160]
                 , [120,163,120,135,143,136,144,120]
                 , (27, -9)
                 )
               , ( [540,580,600,680,430,740,600,690,605,520]
                 , [760,710,1105,880,500,990,1050,640,595,520]
                 , (3, -42)
                 )
               ]
    to4dp tgt x = x >= tgt - 0.00005 && x < tgt + 0.00005
