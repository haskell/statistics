{-# LANGUAGE FlexibleContexts #-}
-- | t-test is a parametric test for assesing the difference of the means between two samples.
module Statistics.Test.StudentT (
  tTest2,
  TTestMode(..),
  TestResult(..),
  TestType(..)
) where

import Statistics.Distribution hiding (mean)
import Statistics.Distribution.StudentT
import Statistics.Sample (mean, varianceUnbiased)
import Statistics.Test.Types
import qualified Data.Vector.Generic as G

data TTestMode = Student  -- ^ Student's t-test
               | Welch    -- ^ Welch's t-test
               | Paired   -- ^ paired-sample t-test
               deriving Eq

-- |Two-sample t-test.
--
-- In two-sample t-test, you can choose a test mode based on the experimental design:
--
--  [@Student@] Student's t-test, where variance equality of two samples is assumed.
--  [@Welch@]   Welch's t-test, where variance equality of two samples is not assumed.
--  [@Paired@]  paired t-test, where two samples are paired in a within-subject design.
--
tTest2 :: (G.Vector v Double)
      => TestType       -- ^ one- or two-tailed test
      -> Double         -- ^ p-value
      -> TTestMode      -- ^ t-test mode (Student, Welch or Paired)
      -> v Double       -- ^ Sample 1
      -> v Double       -- ^ Smaple 2
      -> TestResult
tTest2 test p mode sample1 sample2 = significant $ pvalue < p
  where
    pvalue = case test of
      OneTailed -> tailArea
      TwoTailed -> tailArea * 2
    (t, df) = tStatistics mode sample1 sample2
    tailArea = let area = cumulative (studentT df) t
      in if t < 0 then area else 1 - area

tStatistics :: (G.Vector v Double) => TTestMode -> v Double -> v Double -> (Double, Double)
tStatistics mode sample1 sample2
  | G.null sample1 || G.null sample2 = error "Statistics.Test.StudentT: empty sample"
  | mode == Paired && n1 /= n2       = error "Statistics.Test.StudentT: pair mismatch"
  | otherwise                        = (t, df)
  where
    -- t-statistics
    t = case mode of
      Student -> (m1 - m2) / sqrt (s12 * (1 / n1 + 1 / n2))
      Welch   -> (m1 - m2) / sqrt (s1 / n1 + s2 / n2)
      Paired  ->
        let
          d = G.zipWith (-) sample1 sample2
          sumd = G.sum d
        in sumd / sqrt ((n1 * G.sum (G.map sq d) - sq sumd) / (n1 - 1))

    -- degree of freedom
    df = case mode of
      Student -> n12 - 2
      Welch   -> sq (s1 / n1 + s2 / n2) / (sq s1 / (sq n1 * (n1 - 1)) + sq s2 / (sq n2 * (n2 - 1)))
      Paired  -> n1 - 1

    sq x = x * x

    -- statistics of two samples
    n1 = fromIntegral $ G.length sample1
    n2 = fromIntegral $ G.length sample2
    n12 = n1 + n2
    m1 = mean sample1
    m2 = mean sample2
    s1 = varianceUnbiased sample1
    s2 = varianceUnbiased sample2
    s12 = ((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2)
