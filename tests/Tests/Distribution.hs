{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-- Required for Param
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Tests.Distribution (
    distributionTests
  ) where

import Control.Applicative
import Control.Exception

import Data.List     (find)
import Data.Typeable (Typeable)

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
  , contDistrTests (T :: T FDistribution           )

  , discreteDistrTests (T :: T BinomialDistribution       )
  , discreteDistrTests (T :: T GeometricDistribution      )
  , discreteDistrTests (T :: T HypergeometricDistribution )
  , discreteDistrTests (T :: T PoissonDistribution        )

  , unitTests
  ]

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- Tests for continous distribution
contDistrTests :: ( Param d, ContDistr d, QC.Arbitrary d, Typeable d
                  , Show d, DistrSample d ~ Double)
               => T d -> Test
contDistrTests t = testGroup ("Tests for: " ++ typeName t) $
  cdfTests t ++
  [ testProperty "PDF sanity"              $ pdfSanityCheck   t
  , testProperty "Quantile is CDF inverse" $ quantileIsInvCDF t
  , testProperty "quantile fails p<0||p>1" $ quantileShouldFail t
  ]

-- Tests for discrete distribution
discreteDistrTests :: (Param d, DiscreteDistr d, QC.Arbitrary d, Typeable d, Show d) => T d -> Test
discreteDistrTests t = testGroup ("Tests for: " ++ typeName t) $
  cdfTests t ++
  [ testProperty "Prob. sanity"         $ probSanityCheck       t
  , testProperty "CDF is sum of prob."  $ discreteCDFcorrect    t
  ]

-- Tests for distributions which have CDF
cdfTests :: (Param d, Distribution d, QC.Arbitrary d, Show d) => T d -> [Test]
cdfTests t =
  [ testProperty "C.D.F. sanity"        $ cdfSanityCheck         t
  , testProperty "CDF limit at +∞"      $ cdfLimitAtPosInfinity  t
  , testProperty "CDF limit at -∞"      $ cdfLimitAtNegInfinity  t
  , testProperty "CDF is nondecreasing" $ cdfIsNondecreasing     t
  , testProperty "1-CDF is correct"     $ cdfComplementIsCorrect t
  ]
----------------------------------------------------------------

-- CDF is in [0,1] range
cdfSanityCheck :: (Distribution d) => T d -> d -> Double -> Bool
cdfSanityCheck _ d x = c >= 0 && c <= 1 
  where c = cumulative d x

-- CDF never decreases
cdfIsNondecreasing :: (Distribution d) => T d -> d -> Double -> Double -> Bool
cdfIsNondecreasing _ d = monotonicallyIncreasesIEEE $ cumulative d

-- CDF limit at +∞ is 1
cdfLimitAtPosInfinity :: (Param d, Distribution d) => T d -> d -> Property
cdfLimitAtPosInfinity _ d =
  okForInfLimit d ==> printTestCase ("Last elements: " ++ show (drop 990 probs))
                    $ Just 1.0 == (find (>=1) probs)
  where
    probs = take 1000 $ map (cumulative d) $ iterate (*1.4) 1

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


-- PDF is positive
pdfSanityCheck :: (ContDistr d, DistrSample d ~ Double) => T d -> d -> Double -> Bool
pdfSanityCheck _ d x = p >= 0
  where p = density d x

-- Quantile is inverse of CDF
quantileIsInvCDF :: (Param d, ContDistr d, DistrSample d ~ Double) => T d -> d -> Double -> Property
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
  = printTestCase (printf "CDF = %g" p1)
  $ printTestCase (printf "Sum = %g" p2)
  $ printTestCase (printf "Δ   = %g" (abs (p1 - p2)))
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
      = testAssertion (printf "density (studentT %f) %f ≈ %f" ndf x exact)
      $ eq 1e-5  exact  (density (studentT ndf) x)
    testStudentCDF ndf x exact
      = testAssertion (printf "cumulative (studentT %f) %f ≈ %f" ndf x exact)
      $ eq 1e-5  exact  (cumulative (studentT ndf) x)
    -- F-distribution
    testFdistrPDF n m x exact
      = testAssertion (printf "density (fDistribution %i %i) %f ≈ %f [got %f]" n m x exact d)
      $ eq 1e-5  exact d
      where d = density (fDistribution n m) x
    testFdistrCDF n m x exact
      = testAssertion (printf "cumulative (fDistribution %i %i) %f ≈ %f [got %f]" n m x exact d)
      $ eq 1e-5  exact d
      where d = cumulative (fDistribution n m) x
