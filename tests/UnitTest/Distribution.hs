module UnitTest.Distribution where

import Test.HUnit
import Statistics.Constants
import Statistics.Distribution
import Statistics.Distribution.Gamma

-- Approximate equality
eq :: Double -> Double -> Double -> Bool
eq eps a b = abs (a - b) / max (abs a) (abs b) <= eps

-- Approximately equal up to 1 ulp
(=~) :: Double -> Double -> Bool
(=~) = eq m_epsilon

distributionTests :: [Test]
distributionTests =
  [ TestCase $ assertBool "density (gammaDistr 150 1/150) 1 == 4.883311" $
      4.883311418525483 =~ (density (gammaDistr 150 (1/150)) 1)
  ]
