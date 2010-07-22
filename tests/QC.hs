import Control.Applicative
import qualified Data.Vector.Unboxed as U
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Modifiers (Positive(..))

import Statistics.Constants (m_epsilon,m_pos_inf)
import Statistics.Math
import Statistics.Distribution
import Statistics.Distribution.Binomial
import Statistics.Distribution.ChiSquared
import Statistics.Distribution.Exponential
import Statistics.Distribution.Gamma
import Statistics.Distribution.Geometric
import Statistics.Distribution.Hypergeometric
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Approximate equality (1e-6 is arbitrary)
eq :: Double -> Double -> Bool
eq x y = abs (x - y) < 1e-6

p :: QC.Testable prop => prop -> IO ()
p = QC.quickCheck

runTests :: [(String, IO ())] -> IO ()
runTests = mapM_ $ \(name, test) -> putStrLn (" * " ++ name) >> test

-- Double in [0,1] range
newtype OI = OI Double deriving Show
instance QC.Arbitrary OI where
  arbitrary = OI <$> QC.choose (0,1)

-- Double in [0,100] range
newtype ReasonablyPositive = ReasonablyPositive Double deriving Show
instance QC.Arbitrary ReasonablyPositive where
  arbitrary = ReasonablyPositive <$> QC.choose (0,100)

----------------------------------------------------------------
-- Chebyshev
----------------------------------------------------------------

-- | Chebyshev polynomials
ch0,ch1,ch2,ch3,ch4 :: Double -> Double
ch0 _ = 1
ch1 x = x
ch2 x = 2*x^2 - 1
ch3 x = 4*x^3 - 3*x
ch4 x = 8*x^4 - 8*x^2 + 1

-- | Test correctness of S.Math.chebysev
testChebyshev :: [(String,IO ())]
testChebyshev =
    [ ("==== Checbyshev polynomials ====", return ())
    , ("Deg. 0", p (\a0 y ->
                     let x = frac y
                     in eq (ch0 x * a0) (chebyshev x $ U.fromList [a0])))
    , ("Deg. 1", p (\a0 a1 y ->
                     let x = frac y
                     in eq (a0*ch0 x + a1*ch1 x) (chebyshev x $ U.fromList [a0,a1])))
    , ("Deg. 2", p (\a0 a1 a2 y ->
                     let x = frac y
                     in eq (a0*ch0 x + a1*ch1 x + a2*ch2 x) (chebyshev x $ U.fromList [a0,a1,a2])))
    , ("Deg. 3", p (\a0 a1 a2 a3 y ->
                     let x = frac y
                     in eq (a0*ch0 x + a1*ch1 x + a2*ch2 x + a3*ch3 x)
                           (chebyshev x $ U.fromList [a0,a1,a2,a3])))
    , ("Deg. 4", p (\a0 a1 a2 a3 a4 y ->
                     let x = frac y
                     in eq (a0*ch0 x + a1*ch1 x + a2*ch2 x + a3*ch3 x + a4*ch4 x)
                           (chebyshev x $ U.fromList [a0,a1,a2,a3,a4])))
    ]
    where frac x = (x - fromIntegral (floor x)) * 2 - 1

----------------------------------------------------------------
-- Special functions
----------------------------------------------------------------

-- Check that function is monotonically increasing
monotonityCheck :: (Double -> Double) -> Double -> Double -> Bool
monotonityCheck f x1 x2 = f (min x1 x2) <= f (max x1 x2)

-- Check that function reach limit at positive infinity
limitAtInfinityCheck :: (Double -> Double) -- Function
                     -> (Double,Double)    -- Limit value and precision
                     -> (Double,Int)       -- Starting point and maximum number of iterations
                     -> Bool
limitAtInfinityCheck f (lim,ε) (x0,n) = length (dropWhile (not . atLimit) values) > 1
  where
    values    = take n . map f $ iterate (*2) x0
    atLimit x = abs(lim - x) <= ε

-- γ(1,x) = 1 - exp(-x)
-- Since Γ(1) = 1 normalization doesn't make any difference
incompleteGammaAt1Check :: Double -> Bool
incompleteGammaAt1Check x = (abs $ (incompleteGamma 1 x) - (1 - exp(-x))) < 1e-12

-- Γ(x+1) = x·Γ(x)
gammaErr :: (Double -> Double) -> Double -> Double
gammaErr logG x = let g1 = logG x
                      g2 = logG (x+1)
                  in (g2 - g1 - log x)

-- Check for error
gammaTest :: (Double -> Double) -> Double -> ReasonablyPositive -> Bool
gammaTest logG ε (ReasonablyPositive x) = abs (gammaErr logG x) < ε

testSpecFun :: [(String,IO ())]
testSpecFun =
  [ ("==== Special function ====", return ())
  , ( "gamma",  p $ gammaTest logGamma  3e-8 )
  , ( "gammaL", p $ gammaTest logGammaL 2e-13)
  , ( "incompleteGamma increasing"
    , p (\(Positive s) (Positive x1) (Positive x2) -> monotonityCheck (incompleteGamma s) x1 x2))
  , ( "incompleteGamma limit at +∞"
    , p (\(Positive s) -> limitAtInfinityCheck (incompleteGamma s) (1.0,0.0) (s,100)))
  , ( "γ(1,x) = 1 - exp(-x)", p (\(Positive x) -> incompleteGammaAt1Check x))
  ]

----------------------------------------------------------------
-- Tests for probabilites distributions
----------------------------------------------------------------

-- Arbitrary instances for ditributions
instance QC.Arbitrary BinomialDistribution where
  arbitrary = binomial <$> QC.choose (1,100) <*> QC.choose (0,1)
instance QC.Arbitrary ExponentialDistribution where
  arbitrary = exponential <$> QC.choose (0,100)
instance QC.Arbitrary GammaDistribution where
  arbitrary = gammaDistr <$> QC.choose (0.1,10) <*> QC.choose (0.1,10)
instance QC.Arbitrary GeometricDistribution where
  arbitrary = geometric <$> QC.choose (0,1)
instance QC.Arbitrary HypergeometricDistribution where
  arbitrary = do l <- QC.choose (1,20)
                 m <- QC.choose (0,l)
                 k <- QC.choose (1,l)
                 return $ hypergeometric m l k
instance QC.Arbitrary NormalDistribution where
  arbitrary = normalDistr <$> QC.choose (-100,100) <*> QC.choose (1e-3, 1e3)
instance QC.Arbitrary PoissonDistribution where
  arbitrary = poisson <$> QC.choose (0,1)
instance QC.Arbitrary ChiSquared where
  arbitrary = chiSquared <$> QC.choose (1,100)

-- CDF must be non-decreasing
type CDFMonotonityCheck d = d -> Double -> Double -> Bool
cdfMonotonityCheck :: (Distribution d, QC.Arbitrary d) => CDFMonotonityCheck d
cdfMonotonityCheck d = monotonityCheck (cumulative d)

-- Check tht CDF is in [0,1+16ε] range. 16ε is to accept roundoff
-- errors. 16 is arbitrary value.
--
-- ATTENTION! remove checks for roundoff errors in the S.Distribution
-- before runnning test
type CDFSanityCheck d = (d,Double) -> Bool
cdfSanityCheck :: (Distribution d, QC.Arbitrary d) => CDFSanityCheck d
cdfSanityCheck (d,x) = c >= 0 && c <= (1 + 16*m_epsilon) where c = cumulative d x

type PDFSanityCheck d = (d,Double) -> Bool
pdfSanityCheck :: (ContDistr d, QC.Arbitrary d) => PDFSanityCheck d
pdfSanityCheck (d,x) = p >= 0 && p < m_pos_inf where p = density d x

type ProbSanityCheck d = (d,Int) -> Bool
probSanityCheck :: (DiscreteDistr d, QC.Arbitrary d) => ProbSanityCheck d
probSanityCheck (d,x) = p >=0 && p <= 1 where p = probability d x

-- | Tests for distributions
testDistr :: [(String, IO ())]
testDistr =
  [ ("==== CDF sanity checks ====", return ())
  , ("Binomial",       p (cdfSanityCheck :: CDFSanityCheck BinomialDistribution))
  , ("ChiSquared",     p (cdfSanityCheck :: CDFSanityCheck ChiSquared))
  , ("Exponential",    p (cdfSanityCheck :: CDFSanityCheck ExponentialDistribution))
  , ("Gamma",          p (cdfSanityCheck :: CDFSanityCheck GammaDistribution))
  , ("Geometric",      p (cdfSanityCheck :: CDFSanityCheck GeometricDistribution))
  , ("Hypergeometric", p (cdfSanityCheck :: CDFSanityCheck HypergeometricDistribution))
  , ("Normal",         p (cdfSanityCheck :: CDFSanityCheck NormalDistribution))
  , ("Poisson",        p (cdfSanityCheck :: CDFSanityCheck PoissonDistribution))

  , ("==== CDF monotonity checks ====", return ())
  , ("Binomial",       p (cdfMonotonityCheck :: CDFMonotonityCheck BinomialDistribution))
  , ("ChiSquared",     p (cdfMonotonityCheck :: CDFMonotonityCheck ChiSquared))
  , ("Exponential",    p (cdfMonotonityCheck :: CDFMonotonityCheck ExponentialDistribution))
  , ("Gamma",          p (cdfMonotonityCheck :: CDFMonotonityCheck GammaDistribution))
  , ("Geometric",      p (cdfMonotonityCheck :: CDFMonotonityCheck GeometricDistribution))
  , ("Hypergeometric", p (cdfMonotonityCheck :: CDFMonotonityCheck HypergeometricDistribution))
  , ("Normal",         p (cdfMonotonityCheck :: CDFMonotonityCheck NormalDistribution))
  , ("Poisson",        p (cdfMonotonityCheck :: CDFMonotonityCheck PoissonDistribution))

  , ("==== PDF sanity checks ====", return ())
  , ("Exponential",    p (pdfSanityCheck :: PDFSanityCheck ExponentialDistribution))
  , ("Gamma",          p (pdfSanityCheck :: PDFSanityCheck GammaDistribution))
  , ("Normal",         p (pdfSanityCheck :: PDFSanityCheck NormalDistribution))
  , ("ChiSquared",     p (pdfSanityCheck :: PDFSanityCheck ChiSquared))

  , ("==== Probability sanity check ====", return ())
  , ("Binomial",       p (probSanityCheck :: ProbSanityCheck BinomialDistribution))
  , ("Geometric",      p (probSanityCheck :: ProbSanityCheck GeometricDistribution))
  , ("Hypergeometric", p (probSanityCheck :: ProbSanityCheck HypergeometricDistribution))
  , ("Poisson",        p (probSanityCheck :: ProbSanityCheck PoissonDistribution))

  ]


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Complete list of tests
testAll :: [(String, IO())]
testAll = concat [ testChebyshev
                 , testSpecFun
                 , testDistr
                 ]

main :: IO ()
main = runTests testAll