{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Orphan instances for common data types
module Tests.Orphanage where

import Control.Applicative
import Statistics.Distribution.Beta            (BetaDistribution, betaDistr)
import Statistics.Distribution.Binomial        (BinomialDistribution, binomial)
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.ChiSquared      (ChiSquared, chiSquared)
import Statistics.Distribution.Exponential     (ExponentialDistribution, exponential)
import Statistics.Distribution.FDistribution   (FDistribution, fDistribution)
import Statistics.Distribution.Gamma           (GammaDistribution, gammaDistr)
import Statistics.Distribution.Geometric
import Statistics.Distribution.Hypergeometric
import Statistics.Distribution.Laplace         (LaplaceDistribution, laplace)
import Statistics.Distribution.Lognormal       (LognormalDistribution, lognormalDistr)
import Statistics.Distribution.Normal          (NormalDistribution, normalDistr)
import Statistics.Distribution.Poisson         (PoissonDistribution, poisson)
import Statistics.Distribution.StudentT
import Statistics.Distribution.Transform       (LinearTransform, scaleAround)
import Statistics.Distribution.Uniform         (UniformDistribution, uniformDistr)
import Statistics.Distribution.Weibull         (WeibullDistribution, weibullDistr)
import Statistics.Distribution.DiscreteUniform (DiscreteUniform, discreteUniformAB)
import Statistics.Types

import Test.QuickCheck         as QC


----------------------------------------------------------------
-- Arbitrary instances for ditributions
----------------------------------------------------------------

instance QC.Arbitrary BinomialDistribution where
  arbitrary = binomial <$> QC.choose (1,100) <*> QC.choose (0,1)
instance QC.Arbitrary ExponentialDistribution where
  arbitrary = exponential <$> QC.choose (0,100)
instance QC.Arbitrary LaplaceDistribution where
  arbitrary = laplace <$> QC.choose (-10,10) <*> QC.choose (0, 2)
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
instance QC.Arbitrary LognormalDistribution where
  -- can't choose sigma too big, otherwise goes outside of double-float limit
  arbitrary = lognormalDistr <$> QC.choose (-100,100) <*> QC.choose (1e-10, 20)
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
instance QC.Arbitrary WeibullDistribution where
  arbitrary = weibullDistr <$> QC.choose (1e-3,1e3) <*> QC.choose (1e-3, 1e3)
instance QC.Arbitrary CauchyDistribution where
  arbitrary = cauchyDistribution
                <$> arbitrary
                <*> ((abs <$> arbitrary) `suchThat` (> 0))
instance QC.Arbitrary StudentT where
  arbitrary = studentT <$> ((abs <$> arbitrary) `suchThat` (>0))
instance QC.Arbitrary d => QC.Arbitrary (LinearTransform d) where
  arbitrary = do
    m <- QC.choose (-10,10)
    s <- QC.choose (1e-1,1e1)
    d <- arbitrary
    return $ scaleAround m s d
instance QC.Arbitrary FDistribution where
  arbitrary =  fDistribution
           <$> ((abs <$> arbitrary) `suchThat` (>0))
           <*> ((abs <$> arbitrary) `suchThat` (>0))


instance (Arbitrary a, Ord a, RealFrac a) => Arbitrary (PValue a) where
  arbitrary = do
    (_::Int,x) <- properFraction <$> arbitrary
    return $ mkPValue $ abs x

instance (Arbitrary a, Ord a, RealFrac a) => Arbitrary (CL a) where
  arbitrary = do
    (_::Int,x) <- properFraction <$> arbitrary
    return $ mkCLFromSignificance $ abs x

instance Arbitrary a => Arbitrary (NormalErr a) where
  arbitrary = NormalErr <$> arbitrary

instance Arbitrary a => Arbitrary (ConfInt a) where
  arbitrary = liftA3 ConfInt arbitrary arbitrary arbitrary

instance (Arbitrary (e a), Arbitrary a) => Arbitrary (Estimate e a) where
  arbitrary = liftA2 Estimate arbitrary arbitrary

instance (Arbitrary a) => Arbitrary (UpperLimit a) where
  arbitrary = liftA2 UpperLimit arbitrary arbitrary

instance (Arbitrary a) => Arbitrary (LowerLimit a) where
  arbitrary = liftA2 LowerLimit arbitrary arbitrary

instance QC.Arbitrary DiscreteUniform where
  arbitrary = discreteUniformAB <$> QC.choose (1,1000) <*> QC.choose(1,1000)
