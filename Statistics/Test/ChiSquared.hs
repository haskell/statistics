{-# LANGUAGE FlexibleContexts #-}
-- | Pearson's chi squared test.
module Statistics.Test.ChiSquared (
    chi2test
  , chi2testCont
  , module Statistics.Test.Types
  ) where

import Prelude hiding (sum)

import Statistics.Distribution
import Statistics.Distribution.ChiSquared
import Statistics.Function        (square)
import Statistics.Sample.Internal (sum)
import Statistics.Test.Types
import Statistics.Types
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U



-- | Generic form of Pearson chi squared tests for binned data. Data
--   sample is supplied in form of tuples (observed quantity,
--   expected number of events). Both must be positive.
--
--   This test should be used only if all bins have expected values of
--   at least 5.
chi2test :: (G.Vector v (Int,Double), G.Vector v Double)
         => Int                 -- ^ Number of additional degrees of
                                --   freedom. One degree of freedom
                                --   is due to the fact that the are
                                --   N observation in total and
                                --   accounted for automatically.
         -> v (Int,Double)      -- ^ Observation and expectation.
         -> Maybe (Test ChiSquared)
chi2test ndf vec
  | ndf <  0  = error $ "Statistics.Test.ChiSquare.chi2test: negative NDF " ++ show ndf
  | n   > 0   = Just Test
              { testSignificance = mkPValue $ complCumulative d chi2
              , testStatistics   = chi2
              , testDistribution = chiSquared n
              }
  | otherwise = Nothing
  where
    n     = G.length vec - ndf - 1
    chi2  = sum $ G.map (\(o,e) -> square (fromIntegral o - e) / e) vec
    d     = chiSquared n
{-# INLINABLE  chi2test #-}
{-# SPECIALIZE
    chi2test :: Int -> U.Vector (Int,Double) -> Maybe (Test ChiSquared) #-}
{-# SPECIALIZE
    chi2test :: Int -> V.Vector (Int,Double) -> Maybe (Test ChiSquared) #-}


-- | Chi squared test for data with normal errors. Data is supplied in
--   form of pair (observation with error, and expectation).
chi2testCont
  :: (G.Vector v (Estimate NormalErr Double, Double), G.Vector v Double)
  => Int                                   -- ^ Number of additional
                                           --   degrees of freedom.
  -> v (Estimate NormalErr Double, Double) -- ^ Observation and expectation.
  -> Maybe (Test ChiSquared)
chi2testCont ndf vec
  | ndf < 0   = error $ "Statistics.Test.ChiSquare.chi2testCont: negative NDF " ++ show ndf
  | n   > 0   = Just Test
              { testSignificance = mkPValue $ complCumulative d chi2
              , testStatistics   = chi2
              , testDistribution = chiSquared n
              }
  | otherwise = Nothing
  where
    n     = G.length vec - ndf - 1
    chi2  = sum $ G.map (\(Estimate o (NormalErr s),e) -> square (o - e) / s) vec
    d     = chiSquared n
