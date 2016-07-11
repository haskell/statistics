module Tests.Regression
       ( tests ) where

import           Data.Vector.Unboxed                  (Vector)
import qualified Data.Vector.Unboxed                  as U
import           Debug.Trace                          (trace)
import           Statistics.Regression
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

data Predictor = Predictor [Vector Double] (Vector Double)
  deriving Show

instance Arbitrary Predictor where
  arbitrary = do
    -- we pick an arbitrary length N - this is the length the
    -- predictor vectors and the responder vector will have
    n <- arbitrary `suchThat` (>1)

    matrix <- listOf1 (vectorOf n $ elements [1,2,3])
              -- the matrix must have more rows than columns
              `suchThat` ((<n) . length)

    responder <- vectorOf n (elements [4,5,6])

    return (Predictor (map U.fromList matrix) (U.fromList responder))

tests :: Test
tests = testGroup "Regression"
        [ testProperty "OLS test - shape" testOLS ]


testOLS :: Predictor -> Bool
testOLS (Predictor matrix responder) =
  let (v,double) = olsRegress matrix responder in
  let vlen = U.length v
      mlen = length matrix in
  if (vlen == mlen+1 && double >=0 && double <= 1)
  then True
  else trace (show ("ols", vlen,mlen,U.length (head matrix), double)) False
