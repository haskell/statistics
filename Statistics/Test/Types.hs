{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Statistics.Test.Types (
    TestType(..)
  , TestResult(..)
  , significant
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable, Data)
import GHC.Generics


-- | Test type. Exact meaning depends on a specific test. But
-- generally it's tested whether some statistics is too big (small)
-- for 'OneTailed' or whether it too big or too small for 'TwoTailed'
data TestType = OneTailed
              | TwoTailed
              deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance FromJSON TestType
instance ToJSON TestType

-- | Result of hypothesis testing
data TestResult = Significant    -- ^ Null hypothesis should be rejected
                | NotSignificant -- ^ Data is compatible with hypothesis
                  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance FromJSON TestResult
instance ToJSON TestResult

-- | Significant if parameter is 'True', not significant otherwiser
significant :: Bool -> TestResult
significant True  = Significant
significant False = NotSignificant
