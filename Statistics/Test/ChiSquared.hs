{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, DeriveGeneric #-}
-- | Pearson's chi squared test.
module Statistics.Test.ChiSquared (
    chi2test
  , Chi2Data(..)
  ) where

import Prelude hiding (sum)

import Statistics.Distribution
import Statistics.Distribution.ChiSquared
import Statistics.Function (square)
import Statistics.Sample.Internal (sum)
import Statistics.Test.Types
import Statistics.Types
import Control.DeepSeq (NFData(..))
import Data.Aeson  (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Data   (Typeable,Data)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)


-- | Generic form of Pearson chi squared tests for binned data. Data
--   sample is supplied in form of tuples (observed quantity,
--   expected number of events). Both must be positive.
chi2test :: (G.Vector v (Int,Double), G.Vector v Double)
         => Int                 -- ^ Number of additional degrees of
                                --   freedom. One degree of freedom
                                --   is due to the fact that the are
                                --   N observation in total and
                                --   accounted for automatically.
         -> v (Int,Double)      -- ^ Observation and expectation.
         -> Maybe (Test Chi2Data)
chi2test ndf vec
  | ndf <  0  = error $ "Statistics.Test.ChiSquare.chi2test: negative NDF " ++ show ndf
  | n > 0     = Just $ Test
              { testSignificance = CL (complCumulative d chi2)
              , testExtraData    = Chi2Data n chi2
              }
  | otherwise = Nothing
  where
    n     = G.length vec - ndf - 1
    chi2  = sum $ G.map (\(o,e) -> square (fromIntegral o - e) / e) vec
    d     = chiSquared n
{-# SPECIALIZE
    chi2test :: Int -> U.Vector (Int,Double) -> Maybe (Test Chi2Data) #-}
{-# SPECIALIZE
    chi2test :: Int -> V.Vector (Int,Double) -> Maybe (Test Chi2Data) #-}


-- | Extra data returned by chi-square test
data Chi2Data = Chi2Data
  { chi2DataNDF  :: !Int         -- ^ Number of degrees of freedom
  , chi2DataChi2 :: !Double      -- ^ Calculated χ²
  }
  deriving (Show,Eq,Typeable,Data,Generic)

instance Binary   Chi2Data
instance FromJSON Chi2Data
instance ToJSON   Chi2Data
instance NFData   Chi2Data
