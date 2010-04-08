import qualified Data.Vector.Unboxed as U
import Statistics.Math

import qualified Test.QuickCheck as QC

-- Approximate equality (1e-6 is arbitrary)
eq :: Double -> Double -> Bool
eq x y = abs (x - y) < 1e-6

p :: QC.Testable prop => prop -> IO ()
p = QC.quickCheck

runTests :: [(String, IO ())] -> IO ()
runTests = mapM_ $ \(name, test) -> putStrLn (" * " ++ name) >> test

----------------------------------------------------------------

-- Chebyshev polynomials
ch0,ch1,ch2,ch3,ch4 :: Double -> Double
ch0 _ = 1
ch1 x = x
ch2 x = 2*x^2 - 1
ch3 x = 4*x^3 - 3*x
ch4 x = 8*x^4 - 8*x^2 + 1

-- Test correctness of S.Math.chebysev
testChebyshev :: [(String,IO ())]
testChebyshev =
    [ ("Checbyshev polynomials", return ())
    , ("Deg. 0", p (\(a0,x) -> eq (ch0 x * a0)
                               (chebyshev x $ U.fromList [a0])))
    , ("Deg. 1", p (\(a0,a1,x) -> eq (a0*ch0 x + a1*ch1 x)
                                  (chebyshev x $ U.fromList [a0,a1])))
    , ("Deg. 2", p (\(a0,a1,a2,x) -> eq (a0*ch0 x + a1*ch1 x + a2*ch2 x)
                                     (chebyshev x $ U.fromList [a0,a1,a2])))
    , ("Deg. 3", p (\((a0,a1,a2),a3,x) -> eq (a0*ch0 x + a1*ch1 x + a2*ch2 x + a3*ch3 x)
                                        (chebyshev x $ U.fromList [a0,a1,a2,a3])))
    , ("Deg. 4", p (\((a0,a1,a2),a3,a4,x) -> eq (a0*ch0 x + a1*ch1 x + a2*ch2 x + a3*ch3 x + a4*ch4 x)
                                           (chebyshev x $ U.fromList [a0,a1,a2,a3,a4])))
    ]
