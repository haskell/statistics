{-#LANGUAGE BangPatterns #-}

module Tests.Correlation
    ( tests ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import qualified Data.Vector as V
import Statistics.Correlation.Kendall

tests :: Test
tests = testGroup "Correlation"
    [ testProperty "Kendall" testKendall
    ]

testKendall :: [(Double, Double)] -> Bool
testKendall xy | isNaN r1 = isNaN r2
               | otherwise = r1 == r2
  where
    r1 = kendallBruteForce xy 
    r2 = kendall $ V.fromList xy

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
                               then (nc, nd, n1, n2)
                               else (nc, nd, n1+1, n2)
                       else (nc, nd, n1, n2+1)
    f (x:xs) = zip (repeat x) xs ++ f xs
    f _ = []
