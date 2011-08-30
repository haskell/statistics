-- import Control.Applicative
-- import Data.List
import Test.HUnit

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U
-- import Statistics.Math
-- import Statistics.Test.NonParametric
-- import Statistics.Distribution
-- import Statistics.Distribution.Gamma
-- import Debug.Trace

import Data.Maybe
import System.Environment
import UnitTest.Distribution
import UnitTest.Math
import UnitTest.NonparametricTest


allTests :: [(String, [Test])]
allTests = [ ("distr",       distributionTests  )
           , ("math",        mathTests          )
           , ("nonparam",    nonparametricTests )
           ]


main :: IO ()
main = do
  args <- getArgs
  let tests = case args of [] -> concatMap snd allTests
                           _  -> concat $ catMaybes $ map (`lookup` allTests) args
  print =<< runTestTT (TestList tests)
