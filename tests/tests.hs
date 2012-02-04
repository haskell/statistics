import Test.Framework       (defaultMain)

import Tests.Distribution
import Tests.NonparametricTest
import qualified Tests.Transform
import qualified Tests.Function

main :: IO ()
main = defaultMain [ distributionTests 
                   , nonparametricTests
                   , Tests.Transform.tests
                   , Tests.Function.tests
                   ]
