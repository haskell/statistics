{-#LANGUAGE BangPatterns #-}

module Tests.Correlation
    ( tests ) where

import Control.Arrow (Arrow(..))
import qualified Data.Vector as V
import Data.Maybe
import Statistics.Correlation
import Statistics.Correlation.Kendall
import Test.Tasty
import Test.Tasty.QuickCheck hiding (sample)
import Test.Tasty.HUnit

import Tests.ApproxEq

----------------------------------------------------------------
-- Tests list
----------------------------------------------------------------

tests :: TestTree
tests = testGroup "Correlation"
    [ testProperty "Pearson correlation"           testPearson
    , testProperty "Spearman correlation is scale invariant" testSpearmanScale
    , testProperty "Spearman correlation, nonlinear"         testSpearmanNonlinear
    , testProperty "Kendall test -- general"       testKendall
    , testCase     "Kendall test -- special cases" testKendallSpecial
    ]


----------------------------------------------------------------
-- Pearson's correlation
----------------------------------------------------------------

testPearson :: [(Double,Double)] -> Property
testPearson sample
  = (length sample > 1 && isJust exact) ==> (case exact of
                                               Just e  -> e ~= fast
                                               Nothing -> property False
                                            )
  where
    (~=) = eql 1e-12
    exact = exactPearson $ map (realToFrac *** realToFrac) sample
    fast  = pearson $ V.fromList sample

exactPearson :: [(Rational,Rational)] -> Maybe Double
exactPearson sample
  | varX == 0 || varY == 0 = Nothing
  | otherwise              = Just $ realToFrac cov / sqrt (realToFrac (varX * varY))
  where
    (xs,ys) = unzip sample
    n       = fromIntegral $ length sample
    -- Mean
    muX  = sum xs / n
    muY  = sum ys / n
    -- Mean of squares
    muX2 = sum (map (\x->x*x) xs) / n
    muY2 = sum (map (\x->x*x) ys) / n
    -- Covariance
    cov  = sum (zipWith (*) [x - muX | x<-xs] [y - muY | y<-ys]) / n
    varX = muX2 - muX*muX
    varY = muY2 - muY*muY

----------------------------------------------------------------
-- Spearman's correlation
----------------------------------------------------------------

-- Test that Spearman correlation is scale invariant
testSpearmanScale :: [(Double,Double)] -> Double -> Property
testSpearmanScale xs a
  = and [ length xs > 1       -- Enough to calculate underflow
        , a /= 0
        , not (isNaN c1)
        , not (isNaN c2)
        , not (isNaN c3)
        , not (isNaN c4)
        ]
  ==> ( counterexample (show xs2)
      $ counterexample (show xs3)
      $ counterexample (show xs4)
      $ counterexample (show (c1,c2,c3,c4))
      $ and [ c1 == c4
           , c1 == signum a * c2
           , c1 == signum a * c3
           ]
      )
  where
    xs2 = map ((*a) *** id  ) xs
    xs3 = map (id   *** (*a)) xs
    xs4 = map ((*a) *** (*a)) xs
    c1 = spearman $ V.fromList xs
    c2 = spearman $ V.fromList xs2
    c3 = spearman $ V.fromList xs3
    c4 = spearman $ V.fromList xs4

-- Test that Spearman correlation allows to transform sample with
testSpearmanNonlinear :: [(Double,Double)] -> Property
testSpearmanNonlinear sample0
  = and [ length sample0 > 1
        , not (isNaN c1)
        , not (isNaN c2)
        , not (isNaN c3)
        , not (isNaN c4)
        ]
  ==> ( counterexample ("S0 = " ++ show sample0)
      $ counterexample ("S1 = " ++ show sample1)
      $ counterexample ("S2 = " ++ show sample2)
      $ counterexample ("S3 = " ++ show sample3)
      $ counterexample ("S4 = " ++ show sample4)
      $ counterexample (show (c1,c2,c3,c4))
      $ and [ c1 == c2
            , c1 == c3
            , c1 == c4
            ]
      )
  where
    -- We need to stretch sample into [-10 .. 10] range to avoid
    -- problems with under/overflows etc.
    stretch xs
      | a == b    = xs
      | otherwise = [ ((x - a)/(b - a) - 0.5) * 20 | x <- xs ]
      where
        a = minimum xs
        b = maximum xs
    sample1 = uncurry zip $ (stretch *** stretch) $ unzip sample0
    sample2 = map (exp *** id ) sample1
    sample3 = map (id  *** exp) sample1
    sample4 = map (exp *** exp) sample1
    c1 = spearman $ V.fromList sample1
    c2 = spearman $ V.fromList sample2
    c3 = spearman $ V.fromList sample3
    c4 = spearman $ V.fromList sample4


----------------------------------------------------------------
-- Kendall's correlation
----------------------------------------------------------------

testKendall :: [(Double, Double)] -> Bool
testKendall xy | isNaN r1 = isNaN r2
               | otherwise = r1 == r2
  where
    r1 = kendallBruteForce xy
    r2 = kendall $ V.fromList xy

testKendallSpecial :: Assertion
testKendallSpecial = vs @=? map (\(xs, ys) -> kendall $ V.fromList $ zip xs ys) d
  where
    (d, vs) = unzip testData
    testData :: [(([Double], [Double]), Double)]
    testData = [ (([1, 2, 3, 1, 2], [1, 2, 1, 5, 2]), -0.375)
               , (([1, 1, 1, 3, 3], [3, 3, 3, 2, 5]), 0)
               ]


kendallBruteForce :: [(Double, Double)] -> Double
kendallBruteForce xy = (n_c - n_d) / sqrt ((n_0 - n_1) * (n_0 - n_2))
  where
    allPairs = f xy
    (n_c, n_d, n_1, n_2) = foldl g (0,0,0,0) allPairs
    n_0 = fromIntegral.length $ allPairs
    g (!nc, !nd, !n1, !n2) ((x1, y1), (x2, y2))
      | (x2 - x1) * (y2 - y1) > 0 = (nc+1, nd, n1, n2)
      | (x2 - x1) * (y2 - y1) < 0 = (nc, nd+1, n1, n2)
      | otherwise = if x1 == x2
                       then if y1 == y2
                               then (nc, nd, n1+1, n2+1)
                               else (nc, nd, n1+1, n2)
                       else (nc, nd, n1, n2+1)
    f (x:xs) = zip (repeat x) xs ++ f xs
    f _ = []
