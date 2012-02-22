module Tests.Function ( tests ) where

import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed   ((!))

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Statistics.Function



tests :: Test
tests = testGroup "S.Function"
  [ testProperty "Sort is sort" p_sort
  ]


p_sort :: [Double] -> Property
p_sort xs =
  not (null xs) ==> U.all (uncurry (<=)) (U.zip v $ U.tail v)
    where
      v = sort $ U.fromList xs

      