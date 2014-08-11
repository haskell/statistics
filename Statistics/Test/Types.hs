{-# LANGUAGE DeriveFunctor, DeriveDataTypeable,DeriveGeneric  #-}
module Statistics.Test.Types (
    Test(..)
  , isSignificant
  , TestResult(..)
  , significant
  , TestType(..)
  , PositionTest(..)
  ) where

import Control.DeepSeq  (NFData(..))
import Data.Aeson       (FromJSON, ToJSON)
import Data.Binary      (Binary)
import Data.Data (Typeable, Data)
import GHC.Generics

import Statistics.Types (CL)


-- | Result of hypothesis testing
data TestResult = Significant    -- ^ Null hypothesis should be rejected
                | NotSignificant -- ^ Data is compatible with hypothesis
                  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance Binary   TestResult
instance FromJSON TestResult
instance ToJSON   TestResult
instance NFData   TestResult



-- | Result of statistical test. It contains test p-value (probability of encountering
data Test d a = Test
  { testSignificance :: !(CL Double)
  , testStatistics   :: !Double
  , testDistribution :: d
  , testExtraData    :: a
  }
  deriving (Eq,Ord,Show,Typeable,Data,Generic,Functor)

instance (Binary   a, Binary   d) => Binary   (Test d a)
instance (FromJSON a, FromJSON d) => FromJSON (Test d a)
instance (ToJSON   a, ToJSON   d) => ToJSON   (Test d a)
instance (NFData   a, NFData   d) => NFData   (Test d a) where
  rnf (Test _ _ a b) = rnf a `seq` rnf b

-- | Check whether test is significant for given p-value.
isSignificant :: CL Double -> Test d a -> TestResult
isSignificant cl t
  -- FIXME: check what Ord instance have correct meaning
  = significant $ cl <= testSignificance t



-- | Test type. Exact meaning depends on a specific test. But
-- generally it's tested whether some statistics is too big (small)
-- for 'OneTailed' or whether it too big or too small for 'TwoTailed'
data TestType = OneTailed
              | TwoTailed
              deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance Binary   TestType
instance FromJSON TestType
instance ToJSON   TestType
instance NFData   TestType

-- | Test type for test which compare positional information of samples.
data PositionTest
  = SamplesDiffer
    -- ^ If test is significant then samples are different.
  | AGreater
    -- ^ If test is significant then first sample greater than second.
  | BGreater
    -- ^ If test is significant then second sample greater than first.
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance Binary   PositionTest
instance FromJSON PositionTest
instance ToJSON   PositionTest
instance NFData   PositionTest

-- | significant if parameter is 'True', not significant otherwiser
significant :: Bool -> TestResult
significant True  = Significant
significant False = NotSignificant
