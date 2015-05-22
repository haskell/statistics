module Tests.Regression
       ( tests ) where

import           Data.Vector.Unboxed                  (Vector)
import qualified Data.Vector.Unboxed                  as U
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
    n <- arbitrary `suchThat` (>0)

    matrix <- listOf1 (vectorOf n $ elements [1,2,3])
              -- the matrix must have more rows than columns
              `suchThat` ((>n) . length)

    responder <- vectorOf n (elements [4,5,6])

    return (Predictor (map U.fromList matrix) (U.fromList responder))

tests :: Test
tests = testGroup "Regression"
        [ testProperty "OLS test - shape" testOLS ]


testOLS :: Predictor -> Bool
testOLS (Predictor matrix responder) =
  let (v,double) = olsRegress matrix responder in
  U.length v == 1 + U.length responder && double >= 0 && double <= 1
