{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-- Required for Param
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Tests.Distribution (
    distributionTests
  ) where

import Control.Applicative
import Control.Exception

import Data.List     (find)
import Data.Typeable (Typeable)
import Data.Binary

import qualified Numeric.IEEE    as IEEE

import Test.Framework                       (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck         as QC
import Test.QuickCheck.Monadic as QC
import Text.Printf

import Statistics.Distribution
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.ChiSquared
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.Exponential
import Statistics.Distribution.FDistribution
import Statistics.Distribution.Gamma
import Statistics.Distribution.Geometric
import Statistics.Distribution.Hypergeometric
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson
import Statistics.Distribution.StudentT
import Statistics.Distribution.Transform
import Statistics.Distribution.Uniform

import Prelude hiding (catch)

import Tests.Helpers


-- | Tests for all distributions
distributionTests :: Test
distributionTests = testGroup "Tests for all distributions"
  [ contDistrTests (T :: T BetaDistribution        )
  , contDistrTests (T :: T CauchyDistribution      )
  , contDistrTests (T :: T ChiSquared              )
  , contDistrTests (T :: T ExponentialDistribution )
  , contDistrTests (T :: T GammaDistribution       )
  , contDistrTests (T :: T NormalDistribution      )
  , contDistrTests (T :: T UniformDistribution     )
  , contDistrTests (T :: T StudentT                )
  , contDistrTests (T :: T (LinearTransform StudentT) )
  , contDistrTests (T :: T FDistribution           )

  , discreteDistrTests (T :: T BinomialDistribution       )
  , discreteDistrTests (T :: T GeometricDistribution      )
  , discreteDistrTests (T :: T GeometricDistribution0     )
  , discreteDistrTests (T :: T HypergeometricDistribution )
  , discreteDistrTests (T :: T PoissonDistribution        )

  , unitTests
  ]

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- Tests for continous distribution
contDistrTests :: (Param d, ContDistr d, QC.Arbitrary d, Typeable d, Show d, Binary d, Eq d) => T d -> Test
contDistrTests t = testGroup ("Tests for: " ++ typeName t) $
  cdfTests t ++
  [ testProperty "PDF sanity"              $ pdfSanityCheck     t
  , testProperty "Quantile is CDF inverse" $ quantileIsInvCDF   t
  , testProperty "quantile fails p<0||p>1" $ quantileShouldFail t
  , testProperty "log density check"       $ logDensityCheck    t
  ]

-- Tests for discrete distribution
discreteDistrTests :: (Param d, DiscreteDistr d, QC.Arbitrary d, Typeable d, Show d, Binary d, Eq d) => T d -> Test
discreteDistrTests t = testGroup ("Tests for: " ++ typeName t) $
  cdfTests t ++
  [ testProperty "Prob. sanity"         $ probSanityCheck       t
  , testProperty "CDF is sum of prob."  $ discreteCDFcorrect    t
  , testProperty "Discrete CDF is OK"   $ cdfDiscreteIsCorrect  t
  , testProperty "log probabilty check" $ logProbabilityCheck   t
  ]

-- Tests for distributions which have CDF
cdfTests :: (Param d, Distribution d, QC.Arbitrary d, Show d, Binary d, Eq d) => T d -> [Test]
cdfTests t =
  [ testProperty "C.D.F. sanity"        $ cdfSanityCheck         t
  , testProperty "CDF limit at +inf"    $ cdfLimitAtPosInfinity  t
  , testProperty "CDF limit at -inf"    $ cdfLimitAtNegInfinity  t
  , testProperty "CDF at +inf = 1"      $ cdfAtPosInfinity       t
  , testProperty "CDF at -inf = 1"      $ cdfAtNegInfinity       t
  , testProperty "CDF is nondecreasing" $ cdfIsNondecreasing     t
  , testProperty "1-CDF is correct"     $ cdfComplementIsCorrect t
  , testProperty "Binary OK"            $ p_binary t
  ]


----------------------------------------------------------------

-- CDF is in [0,1] range
cdfSanityCheck :: (Distribution d) => T d -> d -> Double -> Bool
cdfSanityCheck _ d x = c >= 0 && c <= 1 
  where c = cumulative d x

-- CDF never decreases
cdfIsNondecreasing :: (Distribution d) => T d -> d -> Double -> Double -> Bool
cdfIsNondecreasing _ d = monotonicallyIncreasesIEEE $ cumulative d

-- cumulative d +∞ = 1
cdfAtPosInfinity :: (Param d, Distribution d) => T d -> d -> Bool
cdfAtPosInfinity _ d
  = cumulative d (1/0) == 1

-- cumulative d - ∞ = 0
cdfAtNegInfinity :: (Param d, Distribution d) => T d -> d -> Bool
cdfAtNegInfinity _ d
  = cumulative d (-1/0) == 0

-- CDF limit at +∞ is 1
cdfLimitAtPosInfinity :: (Param d, Distribution d) => T d -> d -> Property
cdfLimitAtPosInfinity _ d =
  okForInfLimit d ==> printTestCase ("Last elements: " ++ show (drop 990 probs))
                    $ Just 1.0 == (find (>=1) probs)
  where
    probs = take 1000 $ map (cumulative d) $ iterate (*1.4) 1000

-- CDF limit at -∞ is 0
cdfLimitAtNegInfinity :: (Param d, Distribution d) => T d -> d -> Property
cdfLimitAtNegInfinity _ d =
  okForInfLimit d ==> printTestCase ("Last elements: " ++ show (drop 990 probs))
                    $ case find (< IEEE.epsilon) probs of
                        Nothing -> False
                        Just p  -> p >= 0
  where
    probs = take 1000 $ map (cumulative d) $ iterate (*1.4) (-1)

-- CDF's complement is implemented correctly
cdfComplementIsCorrect :: (Distribution d) => T d -> d -> Double -> Bool
cdfComplementIsCorrect _ d x = (eq 1e-14) 1 (cumulative d x + complCumulative d x)

-- CDF for discrete distribution uses <= for comparison
cdfDiscreteIsCorrect :: (DiscreteDistr d) => T d -> d -> Property
cdfDiscreteIsCorrect _ d
  = printTestCase (unlines badN)
  $ null badN  
  where
    -- We are checking that:
    --
    -- > CDF(i) - CDF(i-e) = P(i)
    --
    -- Apporixmate equality is tricky here. Scale is set by maximum
    -- value of CDF and probability. Case when all proabilities are
    -- zero should be trated specially.
    badN = [ printf "N=%3i    p[i]=%g\tp[i+1]=%g\tdP=%g\trelerr=%g" i p p1 dp ((p1-p-dp) / max p1 dp)
           | i <- [0 .. 100]
           , let p      = cumulative d $ fromIntegral i - 1e-6
                 p1     = cumulative d $ fromIntegral i
                 dp     = probability d i
                 relerr = ((p1 - p) - dp) / max p1 dp
           ,  not (p == 0 && p1 == 0 && dp == 0)
           && relerr > 1e-14
           ]

logDensityCheck :: (ContDistr d) => T d -> d -> Double -> Property
logDensityCheck _ d x
  = printTestCase (printf "density    = %g" p)
  $ printTestCase (printf "logDensity = %g" logP)
  $ printTestCase (printf "log p      = %g" (log p))
  $ printTestCase (printf "eps        = %g" (abs (logP - log p) / max (abs (log p)) (abs logP)))
  $ or [ p == 0     && logP == (-1/0)
       , p < 1e-308 && logP < 609
       , eq 1e-14 (log p) logP
       ]
  where
    p    = density d x
    logP = logDensity d x

-- PDF is positive
pdfSanityCheck :: (ContDistr d) => T d -> d -> Double -> Bool
pdfSanityCheck _ d x = p >= 0
  where p = density d x

-- Quantile is inverse of CDF
quantileIsInvCDF :: (Param d, ContDistr d) => T d -> d -> Double -> Property
quantileIsInvCDF _ d (snd . properFraction -> p) =
  p > 0 && p < 1  ==> ( printTestCase (printf "Quantile     = %g" q )
                      $ printTestCase (printf "Probability  = %g" p )
                      $ printTestCase (printf "Probability' = %g" p')
                      $ printTestCase (printf "Error        = %e" (abs $ p - p'))
                      $ abs (p - p') < invQuantilePrec d
                      )
  where
    q  = quantile   d p
    p' = cumulative d q

-- Test that quantile fails if p<0 or p>1
quantileShouldFail :: (ContDistr d) => T d -> d -> Double -> Property
quantileShouldFail _ d p =
  p < 0 || p > 1 ==> QC.monadicIO $ do r <- QC.run $ catch
                                              (do { return $! quantile d p; return False })
                                              (\(e :: SomeException) -> return True)
                                       QC.assert r


-- Probability is in [0,1] range
probSanityCheck :: (DiscreteDistr d) => T d -> d -> Int -> Bool
probSanityCheck _ d x = p >= 0 && p <= 1 
  where p = probability d x

-- Check that discrete CDF is correct
discreteCDFcorrect :: (DiscreteDistr d) => T d -> d -> Int -> Int -> Property
discreteCDFcorrect _ d a b
  = printTestCase (printf "CDF   = %g" p1)
  $ printTestCase (printf "Sum   = %g" p2)
  $ printTestCase (printf "Delta = %g" (abs (p1 - p2)))
  $ abs (p1 - p2) < 3e-10
  -- Avoid too large differeneces. Otherwise there is to much to sum
  --
  -- Absolute difference is used guard againist precision loss when
  -- close values of CDF are subtracted
  where
    n  = min a b
    m  = n + (abs (a - b) `mod` 100)
    p1 = cumulative d (fromIntegral m + 0.5) - cumulative d (fromIntegral n - 0.5)
    p2 = sum $ map (probability d) [n .. m]

logProbabilityCheck :: (DiscreteDistr d) => T d -> d -> Int -> Property
logProbabilityCheck _ d x
  = printTestCase (printf "probability    = %g" p)
  $ printTestCase (printf "logProbability = %g" logP)
  $ printTestCase (printf "log p          = %g" (log p))
  $ printTestCase (printf "eps            = %g" (abs (logP - log p) / max (abs (log p)) (abs logP)))
  $ or [ p == 0     && logP == (-1/0)
       , p < 1e-308 && logP < 609
       , eq 1e-14 (log p) logP
       ]
  where
    p    = probability d x
    logP = logProbability d x


p_binary :: (Eq a, Show a, Binary a) => T a -> a -> Bool
p_binary _ a = a == (decode . encode) a



----------------------------------------------------------------
-- Arbitrary instances for ditributions
----------------------------------------------------------------

instance QC.Arbitrary BinomialDistribution where
  arbitrary = binomial <$> QC.choose (1,100) <*> QC.choose (0,1)
instance QC.Arbitrary ExponentialDistribution where
  arbitrary = exponential <$> QC.choose (0,100)
instance QC.Arbitrary GammaDistribution where
  arbitrary = gammaDistr <$> QC.choose (0.1,10) <*> QC.choose (0.1,10)
instance QC.Arbitrary BetaDistribution where
  arbitrary = betaDistr <$> QC.choose (1e-3,10) <*> QC.choose (1e-3,10)
instance QC.Arbitrary GeometricDistribution where
  arbitrary = geometric <$> QC.choose (0,1)
instance QC.Arbitrary GeometricDistribution0 where
  arbitrary = geometric0 <$> QC.choose (0,1)
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
instance QC.Arbitrary UniformDistribution where
  arbitrary = do a <- QC.arbitrary
                 b <- QC.arbitrary `suchThat` (/= a)
                 return $ uniformDistr a b
instance QC.Arbitrary CauchyDistribution where
  arbitrary = cauchyDistribution
                <$> arbitrary
                <*> ((abs <$> arbitrary) `suchThat` (> 0))
instance QC.Arbitrary StudentT where
  arbitrary = studentT <$> ((abs <$> arbitrary) `suchThat` (>0))
instance QC.Arbitrary (LinearTransform StudentT) where
  arbitrary = studentTUnstandardized
           <$> ((abs <$> arbitrary) `suchThat` (>0))
           <*> ((abs <$> arbitrary))
           <*> ((abs <$> arbitrary) `suchThat` (>0))
instance QC.Arbitrary FDistribution where
  arbitrary =  fDistribution 
           <$> ((abs <$> arbitrary) `suchThat` (>0))
           <*> ((abs <$> arbitrary) `suchThat` (>0))



-- Parameters for distribution testing. Some distribution require
-- relaxing parameters a bit
class Param a where
  -- Precision for quantileIsInvCDF
  invQuantilePrec :: a -> Double
  invQuantilePrec _ = 1e-14
  -- Distribution is OK for testing limits
  okForInfLimit :: a -> Bool
  okForInfLimit _ = True


instance Param a

instance Param StudentT where
  invQuantilePrec _ = 1e-13
  okForInfLimit   d = studentTndf d > 0.75

instance Param (LinearTransform StudentT) where
  invQuantilePrec _ = 1e-13
  okForInfLimit   d = (studentTndf . linTransDistr) d > 0.75

instance Param FDistribution where
  invQuantilePrec _ = 1e-12



----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

unitTests :: Test
unitTests = testGroup "Unit tests"
  [ testAssertion "density (gammaDistr 150 1/150) 1 == 4.883311" $
      4.883311418525483 =~ (density (gammaDistr 150 (1/150)) 1)
    -- Student-T
  , testStudentPDF 0.3  1.34  0.0648215  -- PDF
  , testStudentPDF 1    0.42  0.27058
  , testStudentPDF 4.4  0.33  0.352994
  , testStudentCDF 0.3  3.34  0.757146   -- CDF
  , testStudentCDF 1    0.42  0.626569
  , testStudentCDF 4.4  0.33  0.621739
    -- Student-T General
  , testStudentUnstandardizedPDF 0.3    1.2  4      0.45 0.0533456  -- PDF
  , testStudentUnstandardizedPDF 4.3  (-2.4) 3.22 (-0.6) 0.0971141
  , testStudentUnstandardizedPDF 3.8    0.22 7.62   0.14 0.0490523
  , testStudentUnstandardizedCDF 0.3    1.2  4      0.45 0.458035   -- CDF
  , testStudentUnstandardizedCDF 4.3  (-2.4) 3.22 (-0.6) 0.698001
  , testStudentUnstandardizedCDF 3.8    0.22 7.62   0.14 0.496076
    -- F-distribution
  , testFdistrPDF  1  3   3     (1/(6 * pi)) -- PDF
  , testFdistrPDF  2  2   1.2   0.206612
  , testFdistrPDF  10 12  8     0.000385613179281892790166
  , testFdistrCDF  1  3   3     0.81830988618379067153 -- CDF
  , testFdistrCDF  2  2   1.2   0.545455
  , testFdistrCDF  10 12  8     0.99935509863451408041
  ]
  where
    -- Student-T
    testStudentPDF ndf x exact
      = testAssertion (printf "density (studentT %f) %f ~ %f" ndf x exact)
      $ eq 1e-5  exact  (density (studentT ndf) x)
    testStudentCDF ndf x exact
      = testAssertion (printf "cumulative (studentT %f) %f ~ %f" ndf x exact)
      $ eq 1e-5  exact  (cumulative (studentT ndf) x)
    -- Student-T General
    testStudentUnstandardizedPDF ndf mu sigma x exact
      = testAssertion (printf "density (studentTUnstandardized %f %f %f) %f ~ %f" ndf mu sigma x exact)
      $ eq 1e-5  exact  (density (studentTUnstandardized ndf mu sigma) x)
    testStudentUnstandardizedCDF ndf mu sigma x exact
      = testAssertion (printf "cumulative (studentTUnstandardized %f %f %f) %f ~ %f" ndf mu sigma x exact)
      $ eq 1e-5  exact  (cumulative (studentTUnstandardized ndf mu sigma) x)
    -- F-distribution
    testFdistrPDF n m x exact
      = testAssertion (printf "density (fDistribution %i %i) %f ~ %f [got %f]" n m x exact d)
      $ eq 1e-5  exact d
      where d = density (fDistribution n m) x
    testFdistrCDF n m x exact
      = testAssertion (printf "cumulative (fDistribution %i %i) %f ~ %f [got %f]" n m x exact d)
      $ eq 1e-5  exact d
      where d = cumulative (fDistribution n m) x
