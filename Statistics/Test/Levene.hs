{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Statistics.Test.Levene
Description : Levene's test for homogeneity of variances.
Copyright   : (c) Praneya Kumar, 2025
License     : BSD-3-Clause

Implements Levene's test to check if multiple groups have equal variances.
Assesses equality of variances, robust to non-normality, and versatile with mean or median centering.
-}
module Statistics.Test.Levene (
    Center(..),
    levenesTest
) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Algorithms.Merge as VA
import Statistics.Distribution (cumulative)
import Statistics.Distribution.FDistribution (fDistribution, FDistribution)
import Statistics.Types (mkPValue)
import Statistics.Test.Types (Test(..))
import qualified Statistics.Sample as Sample
import Control.Exception (assert)

-- | Center calculation method 
data Center = 
    Mean               -- ^ Use arithmetic mean
  | Median             -- ^ Use median
  | Trimmed Double     -- ^ Trimmed mean with given proportion to cut from each end
  deriving (Eq, Show)

-- | Trim data from both ends with error handling and performance optimization
trimboth :: (Ord a, Fractional a) 
         => V.Vector a 
         -> Double 
         -> Either String (V.Vector a)
trimboth vec prop
  | prop < 0 || prop > 1 = Left "Proportion must be between 0 and 1"
  | V.null vec = Right vec
  | otherwise = do
      let sorted = V.modify VA.sort vec
          n = V.length sorted
          lowerCut = floor (prop * fromIntegral n)
          upperCut = n - lowerCut
      assert (upperCut >= lowerCut) $ 
        Right $ V.slice lowerCut (upperCut - lowerCut) sorted

-- | Calculate median using pre-sorted vector
vectorMedian :: (Fractional a, Ord a) 
             => V.Vector a 
             -> Either String a
vectorMedian vec
  | V.null vec = Left "Empty vector in median calculation"
  | otherwise = Right $ 
      if odd len 
        then sorted V.! mid 
        else (sorted V.! (mid - 1) + sorted V.! mid) / 2
  where
    sorted = V.modify VA.sort vec
    len = V.length sorted
    mid = len `div` 2

-- | Main Levene's test function with full error handling
levenesTest :: Double              -- ^ Significance level (alpha)
            -> Center             -- ^ Centering method
            -> [V.Vector Double]  -- ^ Input samples
            -> Either String (Test FDistribution)
levenesTest alpha center samples
  | alpha < 0 || alpha > 1 = Left "Significance level must be between 0 and 1"
  | length samples < 2 = Left "At least two samples required"
  | otherwise = do
      processed <- mapM processSample samples
      let (deviationsList, niList) = unzip processed
          deviations = V.fromList deviationsList  -- V.Vector (U.Vector Double)
          ni = V.fromList niList                  -- V.Vector Int
          zbari = V.map Sample.mean deviations    -- V.Vector Double
          k = V.length deviations
          n = V.sum ni
          zbar = V.sum (V.zipWith (\z n' -> z * fromIntegral n') zbari ni) / fromIntegral n

          -- Numerator: Sum over (ni * (zbari - zbar)^2)
          numerator = V.sum $ V.zipWith (\n z -> fromIntegral n * (z - zbar) ** 2) ni zbari

          -- Denominator: Sum over sum((dev_ij - zbari)^2)
          denominator = V.sum $ V.zipWith (\dev z -> U.sum (U.map (\x -> (x - z) ** 2) dev)) deviations zbari
          
      -- Handle division by zero and invalid values
      if denominator <= 0 || isNaN denominator || isInfinite denominator
        then Left "Invalid denominator in W-statistic calculation"
        else do
          let wStat = (fromIntegral (n - k) / fromIntegral (k - 1)) * (numerator / denominator)
              df1 = k - 1
              df2 = n - k
              fDist = fDistribution df1 df2
              pVal = mkPValue $ 1 - cumulative fDist wStat

          -- Validate distribution parameters
          if df1 < 1 || df2 < 1
            then Left "Invalid degrees of freedom"
            else Right $ Test
                  { testStatistics   = wStat
                  , testSignificance = pVal
                  , testDistribution = fDist
                  }
  where
    -- Process samples with error handling and optimized sorting
    processSample vec = case center of
      Mean -> do
        let dev = V.map (abs . subtract (Sample.mean vec)) vec
        return (U.convert dev, V.length vec)
        
      Median -> do
        sortedVec <- Right $ V.modify VA.sort vec
        m <- vectorMedian sortedVec
        let dev = V.map (abs . subtract m) sortedVec
        return (U.convert dev, V.length vec)
        
      Trimmed p -> do
        trimmed_for_center_calculation <- trimboth vec p
        let robust_center = Sample.mean trimmed_for_center_calculation
            -- Calculate deviations for ALL ORIGINAL points from the robust_center
            deviations_from_robust_center = V.map (abs . subtract robust_center) vec -- Use 'vec' (original data)
        -- Return deviations and the ORIGINAL sample size
        return (U.convert deviations_from_robust_center, V.length vec) -- Use 'V.length vec'


-- Example usage:
-- import qualified Data.Vector as V
-- import LevenesTest (Center(..), levenesTest)
-- import Statistics.Test.Types (testStatistics, testSignificance)
-- import Statistics.Types (pValue)

-- main :: IO ()
-- main = do
--     let a = V.fromList [8.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
--         b = V.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 8.36, 9.18, 8.67, 9.05]
--         c = V.fromList [8.95, 9.12, 8.95, 8.85, 9.03, 8.84, 9.07, 8.98, 8.86, 8.98]

--     case levenesTest (Trimmed 0.05) [a, b, c] of
--         Left err -> putStrLn $ "Error: " ++ err
--         Right test -> do
--             putStrLn $ "Levene's W Statistic: " ++ show (testStatistics test)
--             putStrLn $ "P-Value: " ++ show (pValue (testSignificance test))
--             putStrLn $ "Reject null hypothesis at α=0.05: " ++ show (testSignificance test < 0.05)


-- Sample Output
-- Levene's W Statistic: 7.905
-- P-Value: 0.002
-- Reject null hypothesis at α=0.05: True
