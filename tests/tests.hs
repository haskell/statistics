import Test.Framework       (defaultMain)

import Tests.Distribution
import Tests.Math
import Tests.NonparametricTest
import qualified Tests.Transform
import qualified Tests.Function

main :: IO ()
main = defaultMain [ distributionTests 
                   , mathTests
                   , nonparametricTests
                   , Tests.Transform.tests
                   , Tests.Function.tests
                   ]
