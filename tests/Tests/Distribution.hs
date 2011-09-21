module Tests.Distribution (
    distributionTests
  ) where

import Control.Applicative

import Data.List     (find)
import Data.Typeable (Typeable)

import Test.Framework                      --(defaultMain, testGroup)
-- import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

import Statistics.Distribution
import Statistics.Distribution.Binomial
import Statistics.Distribution.ChiSquared
import Statistics.Distribution.Exponential
import Statistics.Distribution.Gamma
import Statistics.Distribution.Geometric
import Statistics.Distribution.Hypergeometric
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson

import Tests.Helpers

-- | Tests for all distributions
distributionTests :: Test
distributionTests = testGroup "Tests for all distributions"
  [ contDistrTests (T :: T NormalDistribution      )
  , contDistrTests (T :: T ExponentialDistribution )
  , contDistrTests (T :: T GammaDistribution       )
  , contDistrTests (T :: T ChiSquared              )
    
  , discreteDistrTests (T :: T BinomialDistribution       )
  , discreteDistrTests (T :: T GeometricDistribution      )
  -- FIXME: too slow CDF (Could it be fixed???)
  -- , discreteDistrTests (T :: T HypergeometricDistribution )
  -- FIXME: too slow CDF
  -- , discreteDistrTests (T :: T PoissonDistribution        )
  ]

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- Tests for continous distribution
contDistrTests :: (ContDistr d, QC.Arbitrary d, Typeable d, Show d) => T d -> Test
contDistrTests t = testGroup ("Tests for: " ++ typeName t)
  [ testProperty "CDF sanity"           $ cdfSanityCheck        t
  , testProperty "CDF limit at +∞"      $ cdfLimitAtPosInfinity t
  , testProperty "CDF limit at -∞"      $ cdfLimitAtNegInfinity t
  , testProperty "CDF is nondecreasing" $ cdfIsNondecreasing    t
  , testProperty "PDF sanity"           $ pdfSanityCheck        t
  ]

-- Tests for discrete distribution
discreteDistrTests :: (DiscreteDistr d, QC.Arbitrary d, Typeable d, Show d) => T d -> Test
discreteDistrTests t = testGroup ("Tests for: " ++ typeName t)
  [ testProperty "C.D.F. sanity"        $ cdfSanityCheck        t
  , testProperty "CDF limit at +∞"      $ cdfLimitAtPosInfinity t
  , testProperty "CDF limit at -∞"      $ cdfLimitAtNegInfinity t
  , testProperty "CDF is nondecreasing" $ cdfIsNondecreasing    t
  , testProperty "Prob. sanity"         $ probSanityCheck       t
  ]

----------------------------------------------------------------

-- CDF is in [0,1] range
cdfSanityCheck :: (Distribution d, QC.Arbitrary d) => T d -> d -> Double -> Bool
cdfSanityCheck _ d x = c >= 0 && c <= 1 
  where c = cumulative d x

-- CDF never decreases
cdfIsNondecreasing :: (Distribution d, QC.Arbitrary d) => T d -> d -> Double -> Double -> Bool
cdfIsNondecreasing _ d = monotonicallyIncreases $ cumulative d

-- CDF limit at +∞ is 1
cdfLimitAtPosInfinity :: (Distribution d, QC.Arbitrary d) => T d -> d -> Bool
cdfLimitAtPosInfinity _ d = 
  Just 1.0 == (find (>=1) $ take 1000 $ map (cumulative d) $ iterate (*1.4) 1)

-- CDF limit at -∞ is 0
cdfLimitAtNegInfinity :: (Distribution d, QC.Arbitrary d) => T d -> d -> Bool
cdfLimitAtNegInfinity _ d = 
  Just 0.0 == (find (<=0) $ take 1000 $ map (cumulative d) $ iterate (*1.4) (-1))



-- PDF is positive
pdfSanityCheck :: (ContDistr d, QC.Arbitrary d) => T d -> d -> Double -> Bool
pdfSanityCheck _ d x = p >= 0
  where p = density d x

-- Probability is in [0,1] range
probSanityCheck :: (DiscreteDistr d, QC.Arbitrary d) => T d -> d -> Int -> Bool
probSanityCheck _ d x = p >= 0 && p <= 1 
  where p = probability d x


----------------------------------------------------------------
-- Arbitrary instances for ditributions
----------------------------------------------------------------

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
