module Tests.Function ( tests ) where

import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed   ((!))

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Tests.Helpers
import Statistics.Function



tests :: Test
tests = testGroup "S.Function"
  [ testProperty  "Sort is sort"                p_sort
  , testAssertion "nextHighestPowerOfTwo is OK" p_nextHighestPowerOfTwo
  ]


p_sort :: [Double] -> Property
p_sort xs =
  not (null xs) ==> U.all (uncurry (<=)) (U.zip v $ U.tail v)
    where
      v = sort $ U.fromList xs

p_nextHighestPowerOfTwo :: Bool
p_nextHighestPowerOfTwo
  = all (\(good, is) -> all ((==good) . nextHighestPowerOfTwo) is) lists
  where
    pows  = [1 .. 17]
    lists = [ (2^m, [2^n+1 .. 2^m]) | (n,m) <- pows `zip` tail pows ]
