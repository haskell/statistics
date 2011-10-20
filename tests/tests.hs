import Test.Framework       (defaultMain)

import Tests.Distribution
import Tests.Math
import Tests.NonparametricTest
import qualified Tests.Transform

main :: IO ()
main = defaultMain [ distributionTests 
                   , mathTests
                   , nonparametricTests
                   , Tests.Transform.tests
                   ]
