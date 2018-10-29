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

import Control.Monad.Catch (MonadThrow(..))
import Statistics.Distribution hiding (mean)
import Statistics.Distribution.StudentT
import Statistics.Sample  as Sample
import Statistics.Test.Types
import Statistics.Types    (mkPValue,PValue,partial,StatisticsException(..))
import Statistics.Function (square)
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector          as V



-- | Two-sample Student's t-test. It assumes that both samples are
--   normally distributed and have same variance. Returns @Nothing@ if
--   sample sizes are not sufficient.
studentTTest :: (MonadThrow m, G.Vector v Double)
             => PositionTest  -- ^ one- or two-tailed test
             -> v Double      -- ^ Sample A
             -> v Double      -- ^ Sample B
             -> m (Test StudentT)
studentTTest test sample1 sample2
  | G.length sample1 < 2 || G.length sample2 < 2 = throwM $ TestFailure
      "StudentT.studentTTest" "Samples are too small"
  | otherwise                                    = do
      (t, ndf) <- tStatistics True sample1 sample2
      tDistr   <- studentT ndf
      return Test { testSignificance = significance test t tDistr
                  , testStatistics   = t
                  , testDistribution = tDistr
                  }
{-# INLINABLE  studentTTest #-}
{-# SPECIALIZE studentTTest :: MonadThrow m => PositionTest ->
      U.Vector Double -> U.Vector Double -> m (Test StudentT) #-}
{-# SPECIALIZE studentTTest :: MonadThrow m => PositionTest ->
      S.Vector Double -> S.Vector Double -> m (Test StudentT) #-}
{-# SPECIALIZE studentTTest :: MonadThrow m => PositionTest ->
      V.Vector Double -> V.Vector Double -> m (Test StudentT) #-}

-- | Two-sample Welch's t-test. It assumes that both samples are
--   normally distributed but doesn't assume that they have same
--   variance. Returns @Nothing@ if sample sizes are not sufficient.
welchTTest :: (MonadThrow m, G.Vector v Double)
           => PositionTest  -- ^ one- or two-tailed test
           -> v Double      -- ^ Sample A
           -> v Double      -- ^ Sample B
           -> m (Test StudentT)
welchTTest test sample1 sample2
  | G.length sample1 < 2 || G.length sample2 < 2 = throwM $ TestFailure
      "StudentT.welchTTest" "Samples are too small"
  | otherwise                                    = do
      (t, ndf) <- tStatistics False sample1 sample2
      tDistr   <- studentT ndf
      return Test { testSignificance = significance test t tDistr
                  , testStatistics   = t
                  , testDistribution = tDistr
                  }
{-# INLINABLE  welchTTest #-}
{-# SPECIALIZE welchTTest :: MonadThrow m => PositionTest
      -> U.Vector Double -> U.Vector Double -> m (Test StudentT) #-}
{-# SPECIALIZE welchTTest :: MonadThrow m => PositionTest
      -> S.Vector Double -> S.Vector Double -> m (Test StudentT) #-}
{-# SPECIALIZE welchTTest :: MonadThrow m => PositionTest
      -> V.Vector Double -> V.Vector Double -> m (Test StudentT) #-}

-- | Paired two-sample t-test. Two samples are paired in a
-- within-subject design. Returns @Nothing@ if sample size is not
-- sufficient.
pairedTTest :: (MonadThrow m, G.Vector v (Double, Double), G.Vector v Double)
            => PositionTest          -- ^ one- or two-tailed test
            -> v (Double, Double)    -- ^ paired samples
            -> m (Test StudentT)
pairedTTest test sample
  | G.length sample < 2 = throwM $ TestFailure
      "StudentT.pairedTTest" "Sample is too small"
  | otherwise           = do
      tDistr <- studentT ndf
      return Test
        { testSignificance = significance test t tDistr
        , testStatistics   = t
        , testDistribution = tDistr
        }
  where
    (t, ndf) = tStatisticsPaired sample
{-# INLINABLE  pairedTTest #-}
{-# SPECIALIZE pairedTTest :: MonadThrow m => PositionTest -> U.Vector (Double,Double) -> m (Test StudentT) #-}
{-# SPECIALIZE pairedTTest :: MonadThrow m => PositionTest -> V.Vector (Double,Double) -> m (Test StudentT) #-}


-------------------------------------------------------------------------------

significance :: PositionTest    -- ^ one- or two-tailed
             -> Double          -- ^ t statistics
             -> StudentT        -- ^ Student T distribution in question
             -> PValue Double   -- ^ p-value
significance test t tDistr =
  case test of
    -- Here we exploit symmetry of T-distribution and calculate small tail
    SamplesDiffer -> partial $ mkPValue $ 2 * tailArea (negate (abs t))
    AGreater      -> partial $ mkPValue $ tailArea (negate t)
    BGreater      -> partial $ mkPValue $ tailArea  t
  where
    tailArea = cumulative tDistr


-- Calculate T statistics for two samples
tStatistics :: (G.Vector v Double, MonadThrow m)
            => Bool               -- variance equality
            -> v Double
            -> v Double
            -> m (Double, Double)
{-# INLINE tStatistics #-}
tStatistics varequal sample1 sample2 = do
  m1 <- Sample.mean     sample1
  m2 <- Sample.mean     sample2
  s1 <- Sample.variance sample1
  s2 <- Sample.variance sample2
  let -- t-statistics
      t = (m1 - m2) / sqrt (
        if varequal
          then ((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2) * (1 / n1 + 1 / n2)
          else s1 / n1 + s2 / n2)
      -- degree of freedom
      ndf | varequal  = n1 + n2 - 2
          | otherwise = square (s1 / n1 + s2 / n2)
                      / (square s1 / (square n1 * (n1 - 1)) + square s2 / (square n2 * (n2 - 1)))
  return (t, ndf)
  where
    -- statistics of two samples
    n1 = fromIntegral $ G.length sample1
    n2 = fromIntegral $ G.length sample2


-- Calculate T-statistics for paired sample
tStatisticsPaired :: (G.Vector v (Double, Double), G.Vector v Double)
                  => v (Double, Double)
                  -> (Double, Double)
{-# INLINE tStatisticsPaired #-}
tStatisticsPaired sample = (t, ndf)
  where
    -- t-statistics
    t = let d    = G.map (uncurry (-)) sample
            sumd = G.sum d
        in sumd / sqrt ((n * G.sum (G.map square d) - square sumd) / ndf)
    -- degree of freedom
    ndf = n - 1
    n   = fromIntegral $ G.length sample
