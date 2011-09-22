-- | Helpers for testing
module Tests.Helpers (
    -- * helpers
    T(..)
  , typeName
  , eq
  , (=~)
    -- * Generic QC tests
  , monotonicallyIncreases
    -- * HUnit helpers
  , testAssertion
  ) where

import Data.Typeable

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

-- Check that function is monotonically increasing
monotonicallyIncreases :: (Ord a, Ord b) => (a -> b) -> a -> a -> Bool
monotonicallyIncreases f x1 x2 = f (min x1 x2) <= f (max x1 x2)



----------------------------------------------------------------
-- HUnit helpers
----------------------------------------------------------------

testAssertion :: String -> Bool -> Test
testAssertion str cont = testCase str $ HU.assertBool str cont
