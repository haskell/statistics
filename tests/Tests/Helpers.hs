-- | Helpers for testing
module Tests.Helpers (
    -- * helpers
    T(..)
  , typeName
  , eq
  , (=~)
    -- * Generic QC tests
  , monotonicallyIncreases
  , monotonicallyIncreasesIEEE
    -- * HUnit helpers
  , testAssertion
  ) where

import Data.Typeable

import qualified Numeric.IEEE    as IEEE

import qualified Test.HUnit      as HU
import Test.Framework
import Test.Framework.Providers.HUnit

import Statistics.Constants



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Phantom typed value used to select right instance in QC tests
data T a = T

-- | String representation of type name
typeName :: Typeable a => T a -> String
typeName = show . typeOf . typeParam
  where
    typeParam :: T a -> a
    typeParam _ = undefined

eq :: Double -> Double -> Double -> Bool
eq eps a b 
  | a == 0 && b == 0 = True
  | otherwise        = abs (a - b) / max (abs a) (abs b) <= eps

-- Approximately equal up to 1 ulp
(=~) :: Double -> Double -> Bool
(=~) = eq m_epsilon


----------------------------------------------------------------
-- Generic QC
----------------------------------------------------------------

-- Check that function is nondecreasing
monotonicallyIncreases :: (Ord a, Ord b) => (a -> b) -> a -> a -> Bool
monotonicallyIncreases f x1 x2 = f (min x1 x2) <= f (max x1 x2)

-- Check that function is nondecreasing taking rounding errors into
-- account.
--
-- In fact funstion is allowed to decrease less than one ulp in order
-- to guard againist problems with excess precision. On x86 FPU works
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

testAssertion :: String -> Bool -> Test
testAssertion str cont = testCase str $ HU.assertBool str cont
