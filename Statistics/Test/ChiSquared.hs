{-# LANGUAGE FlexibleContexts #-}
-- | Pearson's chi squared test.
module Statistics.Test.ChiSquared (
    chi2test
    -- * Data types
  , TestType(..)
  , TestResult(..)
  ) where

import qualified Data.Vector.Generic as G

import Statistics.Distribution
import Statistics.Distribution.ChiSquared
import Statistics.Test.Types


-- | Generic form of Pearson chi squared tests for binned data. Data
--   sample is supplied in form of tuples (observed quantity,
--   expected number of events). Both must be positive.
chi2test :: (G.Vector v (Int,Double), G.Vector v Double)
         => Double              -- ^ p-value
         -> Int                 -- ^ Number of additional degrees of
                                --   freedom. One degree of freedom
                                --   is due to the fact that the are
                                --   N observation in total and
                                --   accounted for automatically.
         -> v (Int,Double)      -- ^ Observation and expectation.
         -> TestResult
chi2test p ndf vec
  | ndf < 0        = error $ "Statistics.Test.ChiSquare.chi2test: negative NDF " ++ show ndf
  | n   < 0        = error $ "Statistics.Test.ChiSquare.chi2test: too short data sample"
  | p > 0 && p < 1 = significant $ complCumulative d chi2 < p
  | otherwise      = error $ "Statistics.Test.ChiSquare.chi2test: bad p-value: " ++ show p
  where
    n     = G.length vec - ndf - 1
    chi2  = G.sum $ G.map (\(o,e) -> sqr (fromIntegral o - e) / e) vec
    d     = chiSquared n
    sqr x = x * x
{-# INLINE chi2test #-}
