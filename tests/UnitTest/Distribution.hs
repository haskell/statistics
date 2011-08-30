module UnitTest.Distribution where

import Test.HUnit
import Statistics.Distribution
import Statistics.Distribution.Gamma

distributionTests :: [Test]
distributionTests = 
  [TestCase $ assertEqual "density (gammaDistr 150 1/150) 1 == 4.883311" 4.883311418525483 (density (gammaDistr 150 (1/150)) 1) 
  ]

       