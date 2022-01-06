-- |
-- Tests for data serialization instances
module Tests.Serialization where

import Data.Binary (Binary,decode,encode)
import Data.Aeson  (FromJSON,ToJSON,Result(..),toJSON,fromJSON)
import Data.Typeable

import Statistics.Distribution.Beta           (BetaDistribution)
import Statistics.Distribution.Binomial       (BinomialDistribution)
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.ChiSquared     (ChiSquared)
import Statistics.Distribution.Exponential    (ExponentialDistribution)
import Statistics.Distribution.FDistribution  (FDistribution)
import Statistics.Distribution.Gamma          (GammaDistribution)
import Statistics.Distribution.Geometric
import Statistics.Distribution.Hypergeometric
import Statistics.Distribution.Laplace        (LaplaceDistribution)
import Statistics.Distribution.Lognormal      (LognormalDistribution)
import Statistics.Distribution.Normal         (NormalDistribution)
import Statistics.Distribution.Poisson        (PoissonDistribution)
import Statistics.Distribution.StudentT
import Statistics.Distribution.Transform      (LinearTransform)
import Statistics.Distribution.Uniform        (UniformDistribution)
import Statistics.Distribution.Weibull        (WeibullDistribution)
import Statistics.Types

import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck         as QC

import Tests.Helpers
import Tests.Orphanage ()


tests :: TestTree
tests = testGroup "Test for data serialization"
  [ serializationTests (T :: T (CL Float))
  , serializationTests (T :: T (CL Double))
  , serializationTests (T :: T (PValue Float))
  , serializationTests (T :: T (PValue Double))
  , serializationTests (T :: T (NormalErr Double))
  , serializationTests (T :: T (ConfInt   Double))
  , serializationTests' "T (Estimate NormalErr Double)" (T :: T (Estimate NormalErr Double))
  , serializationTests' "T (Estimate ConfInt Double)" (T :: T (Estimate ConfInt   Double))
  , serializationTests (T :: T (LowerLimit Double))
  , serializationTests (T :: T (UpperLimit Double))
    -- Distributions
  , serializationTests (T :: T BetaDistribution        )
  , serializationTests (T :: T CauchyDistribution      )
  , serializationTests (T :: T ChiSquared              )
  , serializationTests (T :: T ExponentialDistribution )
  , serializationTests (T :: T GammaDistribution       )
  , serializationTests (T :: T LaplaceDistribution     )
  , serializationTests (T :: T LognormalDistribution   )
  , serializationTests (T :: T NormalDistribution      )
  , serializationTests (T :: T UniformDistribution     )
  , serializationTests (T :: T WeibullDistribution     )
  , serializationTests (T :: T StudentT                )
  , serializationTests (T :: T (LinearTransform NormalDistribution))
  , serializationTests (T :: T FDistribution           )
  , serializationTests (T :: T BinomialDistribution       )
  , serializationTests (T :: T GeometricDistribution      )
  , serializationTests (T :: T GeometricDistribution0     )
  , serializationTests (T :: T HypergeometricDistribution )
  , serializationTests (T :: T PoissonDistribution        )
  ]


serializationTests
  :: (Eq a, Typeable a, Binary a, Show a, Read a, ToJSON a, FromJSON a, Arbitrary a)
  => T a -> TestTree
serializationTests t = serializationTests' (typeName t) t

-- Not all types are Typeable, unfortunately
serializationTests'
  :: (Eq a, Binary a, Show a, Read a, ToJSON a, FromJSON a, Arbitrary a)
  => String -> T a -> TestTree
serializationTests' name t = testGroup ("Tests for: " ++ name)
  [ testProperty "show/read" (p_showRead t)
  , testProperty "binary"    (p_binary   t)
  , testProperty "aeson"     (p_aeson    t)
  ]



p_binary :: (Eq a, Binary a) => T a -> a -> Bool
p_binary _ a = a == (decode . encode) a

p_showRead :: (Eq a, Read a, Show a) => T a -> a -> Bool
p_showRead _ a = a == (read . show) a

p_aeson :: (Eq a, ToJSON a, FromJSON a) => T a -> a -> Bool
p_aeson _ a = Data.Aeson.Success a == (fromJSON . toJSON) a
