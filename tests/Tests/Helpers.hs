-- | Helpers for testing
module Tests.Helpers (
    -- * QC helpers
    T(..)
  , typeName
  ) where

import Data.Typeable



-- | Phantom typed value used to select right instance in QC tests
data T a = T

-- | String representation of type name
typeName :: Typeable a => T a -> String
typeName = show . typeOf . typeParam
  where
    typeParam :: T a -> a
    typeParam _ = undefined
