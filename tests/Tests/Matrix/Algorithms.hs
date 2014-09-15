{-# LANGUAGE BangPatterns #-}
module Tests.Matrix.Algorithms
    (
      tests
    ) where

import Statistics.Matrix (Matrix(..),fromList)
import Statistics.Matrix.Algorithms (khun_munkres_min)
import Data.List (permutations,nub,foldl')
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2

-- generate a random cost matrix
gen_mat :: (Int,Int) -> Gen Matrix
gen_mat bounds = do
  n <- choose bounds
  fmap (fromList n n . map fromIntegral . concat) $ vectorOf n (vectorOf n (choose (0,100::Int)))

-- compute cost given assignment
value :: Matrix -> U.Vector Int -> Double
value (Matrix dim _ _ v) = U.foldl' (+) 0 . U.unsafeBackpermute v . U.imap (\r c -> r*dim + c)

-- brute-force model to compare with
model :: Matrix -> Double
model (Matrix dim _ _ v) = foldl' (f 0 0) 1e400 . permutations $ [0..dim-1]
    where f !acc !r !cur_min ps | acc >= cur_min = cur_min
                                | r == dim = acc
                                | otherwise = let (p:ps') = ps
                                              in f (acc + U.unsafeIndex v (r*dim+p)) (r+1) cur_min ps'

-- quick check against brute-force model for small examples
prop_khun_min_model_equality :: Property
prop_khun_min_model_equality = forAll (gen_mat (5,10)) $ \m ->
    model m == value m (khun_munkres_min m)

-- check assignment is a bijection
prop_khun_bijection :: Property
prop_khun_bijection = forAll (gen_mat (5,500)) $ \m ->
    rows m == length (nub . filter (>=0) . U.toList $ khun_munkres_min m)

-- all tests
tests :: Test
tests = testGroup "Khun-Munkres" [
    testProperty "min model" prop_khun_min_model_equality
  , testProperty "bijection" prop_khun_bijection
  ]
