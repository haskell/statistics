import Test.Framework       (defaultMain)

import Tests.Distribution
import Tests.Math
import Tests.NonparametricTest


main :: IO ()
main = defaultMain [ distributionTests 
                   , mathTests
                   , nonparametricTests
                   ]
