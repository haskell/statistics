{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Statistics.Test.Bartlett
Description : Bartlett's test for homogeneity of variances.
Copyright   : (c) Praneya Kumar, 2025
License     : BSD-3-Clause

Implements Bartlett's test to check if multiple groups have equal variances.
Assesses equality of variances assuming normal distribution, sensitive to non-normality.
-}
module Statistics.Test.Bartlett (
    bartlettTest,
    module Statistics.Distribution.ChiSquared
) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Statistics.Distribution (cumulative)
import Statistics.Distribution.ChiSquared (chiSquared, ChiSquared(..))
import Statistics.Sample (varianceUnbiased)
import Statistics.Types (mkPValue)
import Statistics.Test.Types (Test(..))

-- | Perform Bartlett's test for equal variances.
-- The input is a list of vectors, where each vector represents a group of observations.
-- Returns Either an error message or a Test ChiSquared containing the test statistic and p-value.
bartlettTest :: [U.Vector Double] -> Either String (Test ChiSquared)
bartlettTest groups
  | length groups < 2 = Left "At least two groups are required for Bartlett's test."
  | any ((< 2) . G.length) groups = Left "Each group must have at least two observations."
  | any (<= 0) groupVariances = Left "All groups must have positive variance."
  | otherwise = Right $ Test
      { testSignificance = pValue
      , testStatistics   = tStatistic
      , testDistribution = chiDist
      }
  where
    -- Number of groups
    k = length groups

    -- Sample sizes for each group
    ni = map G.length groups
    ni' = map fromIntegral ni

    -- Total number of observations across all groups
    nTotal = sum ni

    -- Variance for each group (unbiased estimate)
    groupVariances = map varianceUnbiased groups

    -- Pooled variance calculation
    sumWeightedVars = sum [ (n - 1) * v | (n, v) <- zip ni' groupVariances ]
    pooledVariance = sumWeightedVars / fromIntegral (nTotal - k)

    -- Numerator of Bartlett's statistic
    numerator =
      fromIntegral (nTotal - k) * log pooledVariance -
      sum [ (n - 1) * log v | (n, v) <- zip ni' groupVariances ]

    -- Denominator correction term
    sumReciprocals = sum [1 / (n - 1) | n <- ni']
    denomCorrection =
      1 + (sumReciprocals - 1 / fromIntegral (nTotal - k)) / (3 * (fromIntegral k - 1))

    -- Test statistic T
    tStatistic = max 0 $ numerator / denomCorrection

    -- Degrees of freedom and chi-squared distribution
    df = k - 1
    chiDist = chiSquared df
    pValue = mkPValue $ 1 - cumulative chiDist tStatistic


-- Example usage:
-- import qualified Data.Vector.Unboxed as U
-- import Statistics.Test.Bartlett

-- main :: IO ()
-- main = do
--     let a = U.fromList [8.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
--         b = U.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 8.36, 9.18, 8.67, 9.05]
--         c = U.fromList [8.95, 9.12, 8.95, 8.85, 9.03, 8.84, 9.07, 8.98, 8.86, 8.98]

--     case bartlettTest [a,b,c] of
--         Left err -> putStrLn $ "Error: " ++ err
--         Right test -> do
--             putStrLn $ "Bartlett's Test Statistic: " ++ show (testStatistics test)
--             putStrLn $ "P-Value: " ++ show (testSignificance test)

-- Sample Output
-- Bartlett's Test Statistic: ~32
-- P-Value: ~1e-5
