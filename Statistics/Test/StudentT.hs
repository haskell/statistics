{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
-- | Student's T-test is for assessing whether two samples have
--   different mean. This module contain several variations of
--   T-test. It's a parametric tests and assumes that samples are
--   normally distributed.
module Statistics.Test.StudentT
    (
      studentTTest
    , welchTTest
    , pairedTTest
    , module Statistics.Test.Types
    ) where

import Statistics.Distribution hiding (mean)
import Statistics.Distribution.StudentT
import Statistics.Sample (mean, varianceUnbiased)
import Statistics.Test.Types
import Statistics.Types    (mkPValue,PValue)
import Statistics.Function (square)
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector          as V



-- | Two-sample Student's t-test. It assumes that both samples are
--   normally distributed and have same variance. Returns @Nothing@ if
--   sample sizes are not sufficient.
studentTTest :: (G.Vector v Double)
             => PositionTest  -- ^ one- or two-tailed test
             -> v Double      -- ^ Sample A
             -> v Double      -- ^ Sample B
             -> Maybe (Test StudentT)
studentTTest test sample1 sample2
  | G.length sample1 < 2 || G.length sample2 < 2 = Nothing
  | otherwise                                    = Just Test
      { testSignificance = significance test t ndf
      , testStatistics   = t
      , testDistribution = studentT ndf
      }
  where
    (t, ndf) = tStatistics True sample1 sample2
{-# INLINABLE  studentTTest #-}
{-# SPECIALIZE studentTTest :: PositionTest -> U.Vector Double -> U.Vector Double -> Maybe (Test StudentT) #-}
{-# SPECIALIZE studentTTest :: PositionTest -> S.Vector Double -> S.Vector Double -> Maybe (Test StudentT) #-}
{-# SPECIALIZE studentTTest :: PositionTest -> V.Vector Double -> V.Vector Double -> Maybe (Test StudentT) #-}

-- | Two-sample Welch's t-test. It assumes that both samples are
--   normally distributed but doesn't assume that they have same
--   variance. Returns @Nothing@ if sample sizes are not sufficient.
welchTTest :: (G.Vector v Double)
           => PositionTest  -- ^ one- or two-tailed test
           -> v Double      -- ^ Sample A
           -> v Double      -- ^ Sample B
           -> Maybe (Test StudentT)
welchTTest test sample1 sample2
  | G.length sample1 < 2 || G.length sample2 < 2 = Nothing
  | otherwise                                    = Just Test
      { testSignificance = significance test t ndf
      , testStatistics   = t
      , testDistribution = studentT ndf
      }
  where
    (t, ndf) = tStatistics False sample1 sample2
{-# INLINABLE  welchTTest #-}
{-# SPECIALIZE welchTTest :: PositionTest -> U.Vector Double -> U.Vector Double -> Maybe (Test StudentT) #-}
{-# SPECIALIZE welchTTest :: PositionTest -> S.Vector Double -> S.Vector Double -> Maybe (Test StudentT) #-}
{-# SPECIALIZE welchTTest :: PositionTest -> V.Vector Double -> V.Vector Double -> Maybe (Test StudentT) #-}

-- | Paired two-sample t-test. Two samples are paired in a
-- within-subject design. Returns @Nothing@ if sample size is not
-- sufficient.
pairedTTest :: forall v. (G.Vector v (Double, Double))
            => PositionTest          -- ^ one- or two-tailed test
            -> v (Double, Double)    -- ^ paired samples
            -> Maybe (Test StudentT)
pairedTTest test sample
  | G.length sample < 2 = Nothing
  | otherwise           = Just Test
      { testSignificance = significance test t ndf
      , testStatistics   = t
      , testDistribution = studentT ndf
      }
  where
    (t, ndf) = tStatisticsPaired sample
{-# INLINABLE  pairedTTest #-}
{-# SPECIALIZE pairedTTest :: PositionTest -> U.Vector (Double,Double) -> Maybe (Test StudentT) #-}
{-# SPECIALIZE pairedTTest :: PositionTest -> V.Vector (Double,Double) -> Maybe (Test StudentT) #-}


-------------------------------------------------------------------------------

significance :: PositionTest    -- ^ one- or two-tailed
             -> Double          -- ^ t statistics
             -> Double          -- ^ degree of freedom
             -> PValue Double   -- ^ p-value
significance test t df =
  case test of
    -- Here we exploit symmetry of T-distribution and calculate small tail
    SamplesDiffer -> mkPValue $ 2 * tailArea (negate (abs t))
    AGreater      -> mkPValue $ tailArea (negate t)
    BGreater      -> mkPValue $ tailArea  t
  where
    tailArea = cumulative (studentT df)


-- Calculate T statistics for two samples
tStatistics :: (G.Vector v Double)
            => Bool               -- variance equality
            -> v Double
            -> v Double
            -> (Double, Double)
{-# INLINE tStatistics #-}
tStatistics varequal sample1 sample2 = (t, ndf)
  where
    -- t-statistics
    t = (m1 - m2) / sqrt (
      if varequal
        then ((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2) * (1 / n1 + 1 / n2)
        else s1 / n1 + s2 / n2)

    -- degree of freedom
    ndf | varequal  = n1 + n2 - 2
        | otherwise = square (s1 / n1 + s2 / n2)
                    / (square s1 / (square n1 * (n1 - 1)) + square s2 / (square n2 * (n2 - 1)))
    -- statistics of two samples
    n1 = fromIntegral $ G.length sample1
    n2 = fromIntegral $ G.length sample2
    m1 = mean sample1
    m2 = mean sample2
    s1 = varianceUnbiased sample1
    s2 = varianceUnbiased sample2


-- Calculate T-statistics for paired sample
tStatisticsPaired :: (G.Vector v (Double, Double))
                  => v (Double, Double)
                  -> (Double, Double)
{-# INLINE tStatisticsPaired #-}
tStatisticsPaired sample = (t, ndf)
  where
    -- t-statistics
    t = let d    = U.map (uncurry (-)) $ G.convert sample
            sumd = U.sum d
        in sumd / sqrt ((n * U.sum (U.map square d) - square sumd) / ndf)
    -- degree of freedom
    ndf = n - 1
    n   = fromIntegral $ G.length sample
