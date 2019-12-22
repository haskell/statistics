-- | Tests for Kernel density estimates.
module Tests.KDE (
  tests
  )where

import Data.Vector.Unboxed             ((!))
import Numeric.Sum                     (kbn, sumVector)
import Statistics.Sample.KernelDensity
import Test.Tasty                      (TestTree, testGroup)
import Test.Tasty.QuickCheck           (testProperty)
import Test.QuickCheck                 (Property, (==>), counterexample)
import Text.Printf                     (printf)
import qualified Data.Vector.Unboxed as U


tests :: TestTree
tests = testGroup "KDE"
  [ testProperty "integral(PDF) == 1" t_densityIsPDF
  ]

t_densityIsPDF :: [Double] -> Property
t_densityIsPDF vec
  = not (null vec) ==> test
  where
    (xs,ys)  = kde 4096 (U.fromList vec)
    step     = (xs ! 1) - (xs ! 0)
    integral = integratePDF step ys
    --
    test = counterexample (printf "Integral %f" integral)
         $ abs (1 - integral) <= 1e-3



integratePDF :: Double -> U.Vector Double -> Double
integratePDF step vec
  = step * sumVector kbn (U.zipWith (*) vec weights)
  where
    n       = U.length vec
    weights = U.generate n go
      where
        go i | i == 0    = 0.5
             | i == n-1  = 0.5
             | otherwise = 1
