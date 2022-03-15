{-# LANGUAGE ScopedTypeVariables #-}
-- | Helpers for testing
module Tests.Helpers (
    -- * helpers
    T(..)
  , typeName
  , Double01(..)
    -- * IEEE 754
  , isDenorm
    -- * Generic QC tests
  , monotonicallyIncreases
  , monotonicallyIncreasesIEEE
    -- * HUnit helpers
  , testAssertion
  , testEquality
    -- * QC helpers
  , small
  , unsquare
  , shrinkFixedList
  ) where

import Data.Typeable
import Numeric.MathFunctions.Constants (m_tiny)
import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck
import qualified Numeric.IEEE     as IEEE
import qualified Test.Tasty.HUnit as HU

-- | Phantom typed value used to select right instance in QC tests
data T a = T

-- | String representation of type name
typeName :: Typeable a => T a -> String
typeName = show . typeOf . typeParam
  where
    typeParam :: T a -> a
    typeParam _ = undefined

-- | Check if Double denormalized
isDenorm :: Double -> Bool
isDenorm x = let ax = abs x in ax > 0 && ax < m_tiny

-- | Generates Doubles in range [0,1]
newtype Double01 = Double01 Double
                   deriving (Show)
instance Arbitrary Double01 where
  arbitrary = do
    (_::Int, x) <- fmap properFraction arbitrary
    return $ Double01 x

----------------------------------------------------------------
-- Generic QC
----------------------------------------------------------------

-- Check that function is nondecreasing
monotonicallyIncreases :: (Ord a, Ord b) => (a -> b) -> a -> a -> Bool
monotonicallyIncreases f x1 x2 = f (min x1 x2) <= f (max x1 x2)

-- Check that function is nondecreasing taking rounding errors into
-- account.
--
-- In fact function is allowed to decrease less than one ulp in order
-- to guard against problems with excess precision. On x86 FPU works
-- with 80-bit numbers but doubles are 64-bit so rounding happens
-- whenever values are moved from registers to memory
monotonicallyIncreasesIEEE :: (Ord a, IEEE.IEEE b)  => (a -> b) -> a -> a -> Bool
monotonicallyIncreasesIEEE f x1 x2 =
  y1 <= y2 || (y1 - y2) < y2 * IEEE.epsilon
  where
    y1 = f (min x1 x2)
    y2 = f (max x1 x2)

----------------------------------------------------------------
-- HUnit helpers
----------------------------------------------------------------

testAssertion :: String -> Bool -> TestTree
testAssertion str cont = testCase str $ HU.assertBool str cont

testEquality :: (Show a, Eq a) => String -> a -> a -> TestTree
testEquality msg a b = testCase msg $ HU.assertEqual msg a b

unsquare :: (Arbitrary a, Show a, Testable b) => (a -> b) -> Property
unsquare = forAll (small arbitrary)

small :: Gen a -> Gen a
small act = sized $ \n -> resize (smallish n) act
  where smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs

shrinkFixedList :: (a -> [a]) -> [a] -> [[a]]
shrinkFixedList shr (x:xs) = map (:xs) (shr x) ++ map (x:) (shrinkFixedList shr xs)
shrinkFixedList _   []     = []
