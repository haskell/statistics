-- Tests for Statistics.Test.NonParametric
module Tests.NonParametric (tests) where

import Statistics.Distribution.Normal (standard)
import Statistics.Test.KolmogorovSmirnov
import Statistics.Test.MannWhitneyU
import Statistics.Test.KruskalWallis
import Statistics.Test.WilcoxonT
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)
import Tests.ApproxEq (eq)
import Tests.Helpers (testAssertion, testEquality)
import Tests.NonParametric.Table (tableKSD, tableKS2D)
import qualified Data.Vector.Unboxed as U


tests :: Test
tests = testGroup "Nonparametric tests"
        $ concat [ mannWhitneyTests
                 , wilcoxonSumTests
                 , wilcoxonPairTests
                 , kruskalWallisRankTests
                 , kruskalWallisTests
                 , kolmogorovSmirnovDTest
                 ]

----------------------------------------------------------------

mannWhitneyTests :: [Test]
mannWhitneyTests = zipWith test [(0::Int)..] testData ++
  [ testEquality "Mann-Whitney U Critical Values, m=1"
      (replicate (20*3) Nothing)
      [mannWhitneyUCriticalValue (1,x) p | x <- [1..20], p <- [0.005,0.01,0.025]]
  , testEquality "Mann-Whitney U Critical Values, m=2, p=0.025"
      (replicate 7 Nothing ++ map Just [0,0,0,0,1,1,1,1,1,2,2,2,2])
      [mannWhitneyUCriticalValue (2,x) 0.025 | x <- [1..20]]
  , testEquality "Mann-Whitney U Critical Values, m=6, p=0.05"
      (replicate 1 Nothing ++ map Just [0, 2,3,5,7,8,10,12,14,16,17,19,21,23,25,26,28,30,32])
      [mannWhitneyUCriticalValue (6,x) 0.05 | x <- [1..20]]
  , testEquality "Mann-Whitney U Critical Values, m=20, p=0.025"
      (replicate 1 Nothing ++ map Just [2,8,14,20,27,34,41,48,55,62,69,76,83,90,98,105,112,119,127])
      [mannWhitneyUCriticalValue (20,x) 0.025 | x <- [1..20]]
  ]
  where
    test n (a, b, c, d)
      = testCase "Mann-Whitney" $ do
          assertEqual ("Mann-Whitney U "     ++ show n) c us
          assertEqual ("Mann-Whitney U Sig " ++ show n) d ss
      where
        us = mannWhitneyU (U.fromList a) (U.fromList b)
        ss = mannWhitneyUSignificant TwoTailed (length a, length b) 0.05 us
    -- List of (Sample A, Sample B, (Positive Rank, Negative Rank))
    testData :: [([Double], [Double], (Double, Double), Maybe TestResult)]
    testData = [ ( [3,4,2,6,2,5]
                 , [9,7,5,10,6,8]
                 , (2, 34)
                 , Just Significant
                 )
               , ( [540,480,600,590,605]
                 , [760,890,1105,595,940]
                 , (2, 23)
                 , Just Significant
                 )
               , ( [19,22,16,29,24]
                 , [20,11,17,12]
                 , (17, 3)
                 , Just NotSignificant
                 )
               , ( [126,148,85,61, 179,93, 45,189,85,93]
                 , [194,128,69,135,171,149,89,248,79,137]
                 , (35,65)
                 , Just NotSignificant
                 )
               , ( [1..30]
                 , [1..30]
                 , (450,450)
                 , Just NotSignificant
                 )
               , ( [1 .. 30]
                 , [11.5 .. 40 ]
                 , (190.0,710.0)
                 , Just Significant
                 )
               ]

wilcoxonSumTests :: [Test]
wilcoxonSumTests = zipWith test [(0::Int)..] testData
  where
    test n (a, b, c) = testCase "Wilcoxon Sum"
                     $ assertEqual ("Wilcoxon Sum " ++ show n) c (wilcoxonRankSums (U.fromList a) (U.fromList b))
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
  [ testAssertion "Sig 16, 35" (to4dp 0.0467 $ wilcoxonMatchedPairSignificance 16 35)
  , testAssertion "Sig 16, 36" (to4dp 0.0523 $ wilcoxonMatchedPairSignificance 16 36)
  , testEquality   "Wilcoxon critical values, p=0.05"
      (replicate 4 Nothing ++ map Just [0,2,3,5,8,10,13,17,21,25,30,35,41,47,53,60,67,75,83,91,100,110,119])
      [wilcoxonMatchedPairCriticalValue x 0.05 | x <- [1..27]]
  , testEquality "Wilcoxon critical values, p=0.025"
      (replicate 5 Nothing ++ map Just [0,2,3,5,8,10,13,17,21,25,29,34,40,46,52,58,65,73,81,89,98,107])
      [wilcoxonMatchedPairCriticalValue x 0.025 | x <- [1..27]]
  , testEquality "Wilcoxon critical values, p=0.01"
      (replicate 6 Nothing ++ map Just [0,1,3,5,7,9,12,15,19,23,27,32,37,43,49,55,62,69,76,84,92])
      [wilcoxonMatchedPairCriticalValue x 0.01 | x <- [1..27]]
  , testEquality "Wilcoxon critical values, p=0.005"
      (replicate 7 Nothing ++ map Just [0,1,3,5,7,9,12,15,19,23,27,32,37,42,48,54,61,68,75,83])
      [wilcoxonMatchedPairCriticalValue x 0.005 | x <- [1..27]]
  ]
  where
    test n (a, b, c) = testEquality ("Wilcoxon Paired " ++ show n) c res
      where res = (wilcoxonMatchedPairSignedRank (U.fromList a) (U.fromList b))

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

----------------------------------------------------------------

kruskalWallisRankTests :: [Test]
kruskalWallisRankTests = zipWith test [(0::Int)..] testData
  where
    test n (a, b) = testCase "Kruskal-Wallis Ranking"
                  $ assertEqual ("Kruskal-Wallis " ++ show n) (map U.fromList b) (kruskalWallisRank $ map U.fromList a)
    testData = [ ( [ [68,93,123,83,108,122]
                   , [119,116,101,103,113,84]
                   , [70,68,54,73,81,68]
                   , [61,54,59,67,59,70]
                   ]
                 , [ [8.0,14.0,16.0,19.0,23.0,24.0]
                   , [15.0,17.0,18.0,20.0,21.0,22.0]
                   , [1.5,8.0,8.0,10.5,12.0,13.0]
                   , [1.5,3.5,3.5,5.0,6.0,10.5]
                   ]
                 )
               ]

kruskalWallisTests :: [Test]
kruskalWallisTests = zipWith test [(0::Int)..] testData
  where
    test n (a, b, c) = testCase "Kruskal-Wallis" $ do
        assertEqual ("Kruskal-Wallis " ++ show n) (round100 b) (round100 kw)
        assertEqual ("Kruskal-Wallis Sig " ++ show n) c kwt
      where
        kw = kruskalWallis $ map U.fromList a
        kwt = kruskalWallisTest 0.05 $ map U.fromList a
        round100 :: Double -> Integer
        round100 = round . (*100)

    testData = [ ( [ [68,93,123,83,108,122]
                   , [119,116,101,103,113,84]
                   , [70,68,54,73,81,68]
                   , [61,54,59,67,59,70]
                   ]
                 , 16.03
                 , Just Significant
                 )
               , ( [ [5,5,3,5,5,5,5]
                   , [5,5,5,5,7,5,5]
                   , [5,5,6,5,5,5,5]
                   , [4,5,5,5,6,5,5]
                   ]
               , 2.24
               , Just NotSignificant
               )
               , ( [ [36,48,5,67,53]
                   , [49,33,60,2,55]
                   , [71,31,140,59,42]
                   ]
                 , 1.22
                 , Just NotSignificant
                 )
               , ( [ [6,38,3,17,11,30,15,16,25,5]
                   , [34,28,42,13,40,31,9,32,39,27]
                   , [13,35,19,4,29,0,7,33,18,24]
                   ]
                 , 6.10
                 , Just Significant
                 )
               ]


----------------------------------------------------------------
-- K-S test
----------------------------------------------------------------


kolmogorovSmirnovDTest :: [Test]
kolmogorovSmirnovDTest =
  [ testAssertion "K-S D statistics" $
    and [ eq 1e-6 (kolmogorovSmirnovD standard (toU sample)) reference
        | (reference,sample) <- tableKSD
        ]
  , testAssertion "K-S 2-sample statistics" $
    and [ eq 1e-6 (kolmogorovSmirnov2D (toU xs) (toU ys)) reference
        | (reference,xs,ys) <- tableKS2D
        ]
  , testAssertion "K-S probability" $
    and [ eq 1e-14 (kolmogorovSmirnovProbability n d) p
        | (d,n,p) <- testData
        ]
  ]
  where
    toU = U.fromList
    -- Test data for the calculation of cumulative probability
    -- P(D[n] < d).
    --
    -- Test data is:
    --    (D[n], n, p)
    -- Table is generated using sample program from paper
    testData :: [(Double,Int,Double)]
    testData =
      [ (0.09           ,    3, 0                   )
      , (0.2            ,    3, 0.00177777777777778 )
      , (0.301          ,    3, 0.116357025777778   )
      , (0.392          ,    3, 0.383127210666667   )
      , (0.5003         ,    3, 0.667366306558667   )
      , (0.604          ,    3, 0.861569877333333   )
      , (0.699          ,    3, 0.945458198         )
      , (0.802          ,    3, 0.984475216         )
      , (0.9            ,    3, 0.998               )
      , (0.09           ,    5, 0                   )
      , (0.2            ,    5, 0.0384              )
      , (0.301          ,    5, 0.33993786080016    )
      , (0.392          ,    5, 0.66931908083712    )
      , (0.5003         ,    5, 0.888397260183794   )
      , (0.604          ,    5, 0.971609957879808   )
      , (0.699          ,    5, 0.994331075994008   )
      , (0.802          ,    5, 0.999391366368064   )
      , (0.9            ,    5, 0.99998             )
      , (0.09           ,    8, 3.37615237575e-06   )
      , (0.2            ,    8, 0.151622071801758   )
      , (0.301          ,    8, 0.613891042670582   )
      , (0.392          ,    8, 0.871491561427005   )
      , (0.5003         ,    8, 0.977534089199071   )
      , (0.604          ,    8, 0.997473116268255   )
      , (0.699          ,    8, 0.999806082005123   )
      , (0.802          ,    8, 0.999995133786947   )
      , (0.9            ,    8, 0.99999998          )
      , (0.09           ,   10, 3.89639433093119e-05)
      , (0.2            ,   10, 0.25128096          )
      , (0.301          ,   10, 0.732913126355935   )
      , (0.392          ,   10, 0.932185254518767   )
      , (0.5003         ,   10, 0.992276179340446   )
      , (0.604          ,   10, 0.999495533516769   )
      , (0.699          ,   10, 0.999979691783985   )
      , (0.802          ,   10, 0.999999801409237   )
      , (0.09           ,   20, 0.00794502217168886 )
      , (0.2            ,   20, 0.647279826376584   )
      , (0.301          ,   20, 0.958017466965765   )
      , (0.392          ,   20, 0.997206424843499   )
      , (0.5003         ,   20, 0.999962641414228   )
      , (0.09           ,   30, 0.0498147538075168  )
      , (0.2            ,   30, 0.842030838984526   )
      , (0.301          ,   30, 0.993403560017612   )
      , (0.392          ,   30, 0.99988478803318    )
      , (0.09           ,  100, 0.629367974413669   )
      ]
