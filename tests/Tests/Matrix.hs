module Tests.Matrix (tests) where

import Statistics.Matrix hiding (map)
import Statistics.Matrix.Algorithms
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Tests.ApproxEq (ApproxEq(..))
import Tests.Matrix.Types
import qualified Data.Vector.Unboxed as U

t_row :: Mat Double -> Gen Property
t_row ms@(Mat r _ xs) = do
  i <- choose (0,r-1)
  return $ row (fromMat ms) i === U.fromList (xs !! i)

t_column :: Mat Double -> Gen Property
t_column ms@(Mat _ c xs) = do
  i <- choose (0,c-1)
  return $ column (fromMat ms) i === U.fromList (map (!! i) xs)

t_center :: Mat Double -> Property
t_center ms@(Mat r c xs) =
  (xs !! (r `quot` 2)) !! (c `quot` 2) === center (fromMat ms)

t_transpose :: Matrix -> Property
t_transpose m = U.concat (map (column n) [0..rows m-1]) === toVector m
  where n = transpose m

t_qr :: Matrix -> Property
t_qr a = hasNaN p .||. eql 1e-10 a p
  where p = uncurry multiply (qr a)

tests :: Test
tests = testGroup "Matrix" [
    testProperty "t_row" t_row
  , testProperty "t_column" t_column
  , testProperty "t_center" t_center
  , testProperty "t_transpose" t_transpose
  , testProperty "t_qr" t_qr
  ]
