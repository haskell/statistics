{-# LANGUAGE DeriveFunctor, DeriveDataTypeable,DeriveGeneric  #-}
module Statistics.Test.Types (
    Test(..)
  , isSignificant
  , TestResult(..)
  , significant
  , TestType(..)
  , TestStatistics(..)
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
data Test a = Test
  { testSignificance :: CL Double
  , testExtraData    :: a
  }
  deriving (Eq,Ord,Show,Typeable,Data,Generic,Functor)

instance Binary   a => Binary   (Test a)
instance FromJSON a => FromJSON (Test a)
instance ToJSON   a => ToJSON   (Test a)
instance NFData   a => NFData   (Test a) where
  rnf (Test a b) = rnf a `seq` rnf b

-- | Check whether test is significant for given p-value.
isSignificant :: CL Double -> Test a -> TestResult
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

-- | Significant if parameter is 'True', not significant otherwiser
significant :: Bool -> TestResult
significant True  = Significant
significant False = NotSignificant



-- | Test statistics.
data TestStatistics a = TestStatistics
  { testStatistics   :: !Double
  , testDistribution :: !a
  }
  deriving (Eq,Ord,Show,Typeable,Data,Generic,Functor)

instance Binary   a => Binary   (TestStatistics a)
instance FromJSON a => FromJSON (TestStatistics a)
instance ToJSON   a => ToJSON   (TestStatistics a)
instance NFData   a => NFData   (TestStatistics a) where
  rnf (TestStatistics a b) = rnf a `seq` rnf b
