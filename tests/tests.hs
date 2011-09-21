import Test.Framework       (defaultMain)

import Tests.Distribution
import Tests.Math

main :: IO ()
main = defaultMain [ distributionTests 
                   , mathTests
                   ]
