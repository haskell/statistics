{-#LANGUAGE BangPatterns #-}

module Tests.Correlation
    ( tests ) where

import Control.Arrow (Arrow(..))
import qualified Data.Vector as V
import Statistics.Matrix hiding (map)
import Statistics.Correlation
import Statistics.Correlation.Kendall
import Test.QuickCheck ((==>),Property)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@=?), assertBool)

import Tests.ApproxEq

----------------------------------------------------------------
-- Tests list
----------------------------------------------------------------

tests :: Test
tests = testGroup "Correlation"
    [ testProperty "Pearson correlation"           testPearson
    , testProperty "Kendall test -- general"       testKendall
    , testCase     "Kendall test -- special cases" testKendallSpecial
    ]


----------------------------------------------------------------
-- Pearson's correlation
----------------------------------------------------------------

testPearson :: [(Double,Double)] -> Property
testPearson sample
  = (length sample > 1) ==> (exact ~= fast)
  where
    (~=) = eql 1e-12
    exact = exactPearson $ map (realToFrac *** realToFrac) sample
    fast  = pearson $ V.fromList sample

exactPearson :: [(Rational,Rational)] -> Double
exactPearson sample
  = realToFrac cov / sqrt (realToFrac (varX * varY))
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
