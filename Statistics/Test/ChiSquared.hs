{-# LANGUAGE FlexibleContexts #-}
-- | Pearson's chi squared test.
module Statistics.Test.ChiSquared (
    chi2test
  , Chi2Data(..)
  ) where

import Prelude hiding (sum)

import Statistics.Distribution
import Statistics.Distribution.ChiSquared
import Statistics.Sample.Internal (sum)
import Statistics.Test.Types
import Statistics.Types
import qualified Data.Vector.Generic as G


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
  | n   <= 0  = error $ "Statistics.Test.ChiSquare.chi2test: too short data sample"
  | n > 0     = Just $ Test
              { testSignificance = CL (complCumulative d chi2)
              , testExtraData    = Chi2Data n chi2
              }
  | otherwise = Nothing
  where
    n     = G.length vec - ndf - 1
    chi2  = sum $ G.map (\(o,e) -> sqr (fromIntegral o - e) / e) vec
    d     = chiSquared n
    sqr x = x * x
{-# INLINE chi2test #-}


-- | Extra data returned by chi-square test
data Chi2Data = Chi2Data
  { chi2DataNDF  :: !Int         -- ^ Number of degrees of freedom
  , chi2DataChi2 :: !Double      -- ^ Calculated χ²
  }
