{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
-- | Student's T-test is for assesing whether two samples have
--   different mean. This module contain several variations of
--   T-test. It's a parametric tests and assumes that samples are
--   normally distributed.
module Statistics.Test.StudentT
    (
      studentT2
    , welchT2
    , pairedT2
    , TestResult(..)
    , PositionTest(..)
    ) where

import Statistics.Distribution hiding (mean)
import Statistics.Distribution.StudentT
import Statistics.Sample (mean, varianceUnbiased)
import Statistics.Test.Types
import Statistics.Types    (pValue,CL)
import Statistics.Function (square)
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector          as V



-- | Two-sample Student's t-test. It assumes that both samples are
--   normally distributed and have same variance.
studentT2 :: (G.Vector v Double)
          => PositionTest  -- ^ one- or two-tailed test
          -> v Double      -- ^ Sample A
          -> v Double      -- ^ Sample B
          -> Test StudentT ()
studentT2 test sample1 sample2
  | G.length sample1 < 2 || G.length sample2 < 2 = error "Statistics.Test.StudentT: insufficient sample size"
  | otherwise = Test { testSignificance = significance test t ndf
                     , testStatistics   = t
                     , testDistribution = studentT ndf
                     , testExtraData    = ()
                     }
  where
    (t, ndf) = tStatistics True sample1 sample2
{-# INLINABLE  studentT2 #-}
{-# SPECIALIZE studentT2 :: PositionTest -> U.Vector Double -> U.Vector Double -> Test StudentT () #-}
{-# SPECIALIZE studentT2 :: PositionTest -> S.Vector Double -> S.Vector Double -> Test StudentT () #-}
{-# SPECIALIZE studentT2 :: PositionTest -> V.Vector Double -> V.Vector Double -> Test StudentT () #-}

-- | Two-sample Welch's t-test. It assumes that both samples are
--   normally distributed but doesn't assume that they have same
--   variance.
welchT2 :: (G.Vector v Double)
        => PositionTest  -- ^ one- or two-tailed test
        -> v Double      -- ^ Sample A
        -> v Double      -- ^ Sample B
        -> Test StudentT ()
welchT2 test sample1 sample2
  | G.length sample1 < 2 || G.length sample2 < 2 = error "Statistics.Test.StudentT: insufficient sample size"
  | otherwise = Test { testSignificance = significance test t ndf
                     , testStatistics   = t
                     , testDistribution = studentT ndf
                     , testExtraData    = ()
                     }
  where
    (t, ndf) = tStatistics False sample1 sample2
{-# INLINABLE  welchT2 #-}
{-# SPECIALIZE welchT2 :: PositionTest -> U.Vector Double -> U.Vector Double -> Test StudentT () #-}
{-# SPECIALIZE welchT2 :: PositionTest -> S.Vector Double -> S.Vector Double -> Test StudentT () #-}
{-# SPECIALIZE welchT2 :: PositionTest -> V.Vector Double -> V.Vector Double -> Test StudentT () #-}

-- | Paired two-sample t-test. Two samples are paired in a within-subject design.
pairedT2 :: forall v. (G.Vector v (Double, Double), G.Vector v Double)
         => PositionTest          -- ^ one- or two-tailed test
         -> v (Double, Double)    -- ^ paired samples
         -> Test StudentT ()
pairedT2 test sample
  | G.length sample < 2 = error "Statistics.Test.StudentT: insufficient samples"
  | otherwise = Test { testSignificance = significance test t ndf
                     , testStatistics   = t
                     , testDistribution = studentT ndf
                     , testExtraData    = ()
                     }
  where
    (t, ndf) = tStatisticsPaired sample
{-# INLINABLE  pairedT2 #-}
{-# SPECIALIZE pairedT2 :: PositionTest -> U.Vector (Double,Double) -> Test StudentT () #-}
{-# SPECIALIZE pairedT2 :: PositionTest -> V.Vector (Double,Double) -> Test StudentT () #-}


-------------------------------------------------------------------------------

significance :: PositionTest    -- ^ one- or two-tailed
             -> Double          -- ^ t statistics
             -> Double          -- ^ degree of freedom
             -> CL Double       -- ^ p-value
significance test t df =
  case test of
    -- Here we exploit symmetry of T-distribution and calculate small tail
    SamplesDiffer -> pValue $ 2 * tailArea (negate (abs t))
    AGreater      -> pValue $ tailArea (negate t)
    BGreater      -> pValue $ tailArea  t
  where
    tailArea = cumulative (studentT df)


-- Calculate T statistics
tStatistics :: (G.Vector v Double)
            => Bool               -- variance equality
            -> v Double
            -> v Double
            -> (Double, Double)
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


tStatisticsPaired :: forall v. (G.Vector v (Double, Double), G.Vector v Double)
                  => v (Double, Double)
                  -> (Double, Double)
tStatisticsPaired sample = (t, ndf)
  where
    -- t-statistics
    t = let d    = G.map (uncurry (-)) sample
            sumd = G.sum d
        in sumd / sqrt ((n * G.sum (G.map square d) - square sumd) / ndf)
    -- degree of freedom
    ndf = n - 1
    n   = fromIntegral $ G.length sample
