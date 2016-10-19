{-# LANGUAGE DeriveFunctor, DeriveDataTypeable,DeriveGeneric  #-}
module Statistics.Test.Types (
    Test(..)
  , isSignificant
  , TestResult(..)
  , significant
  , PositionTest(..)
  ) where

import Control.DeepSeq  (NFData(..))
import Data.Aeson       (FromJSON, ToJSON)
import Data.Binary      (Binary)
import Data.Data (Typeable, Data)
import GHC.Generics

import Statistics.Types (PValue,CL,asPValue)


-- | Result of hypothesis testing
data TestResult = Significant    -- ^ Null hypothesis should be rejected
                | NotSignificant -- ^ Data is compatible with hypothesis
                  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance Binary   TestResult
instance FromJSON TestResult
instance ToJSON   TestResult
instance NFData   TestResult



-- | Result of statistical test. It contains test p-value (probability of encountering
data Test distr = Test
  { testSignificance :: !(PValue Double)
  , testStatistics   :: !Double
  , testDistribution :: distr
  }
  deriving (Eq,Ord,Show,Typeable,Data,Generic,Functor)

instance (Binary   d) => Binary   (Test d)
instance (FromJSON d) => FromJSON (Test d)
instance (ToJSON   d) => ToJSON   (Test d)
instance (NFData   d) => NFData   (Test d) where
  rnf (Test _ _ a) = rnf a

-- | Check whether test is significant for given p-value.
isSignificant :: CL Double -> Test d -> TestResult
isSignificant cl t
  = significant $ asPValue cl >= testSignificance t


-- | Test type for test which compare positional (mean,median etc.)
--   information of samples.
data PositionTest
  = SamplesDiffer
    -- ^ Test whether samples differ in position. Null hypothesis is
    --   samples are not different
  | AGreater
    -- ^ Test if first sample (A) is larger than second (B). Null
    --   hypothesis is first sample is not larger than second.
  | BGreater
    -- ^ Test if second sample is larger than first.
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance Binary   PositionTest
instance FromJSON PositionTest
instance ToJSON   PositionTest
instance NFData   PositionTest

-- | significant if parameter is 'True', not significant otherwiser
significant :: Bool -> TestResult
significant True  = Significant
significant False = NotSignificant
