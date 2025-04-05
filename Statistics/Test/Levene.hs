{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
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
    TestResult(..),
    levenesTest
) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Algorithms.Merge as VA
import Statistics.Distribution
import Statistics.Distribution.FDistribution (fDistribution)
import qualified Statistics.Sample as Sample
import Control.Exception (assert)

-- | Center calculation method with proper documentation
data Center = 
    Mean      -- ^ Use arithmetic mean
  | Median    -- ^ Use median
  | Trimmed Double  -- ^ Trimmed mean with given proportion to cut from each end
  deriving (Eq, Show)

-- | Result type with comprehensive information
data TestResult = TestResult {
    wStatistic :: Double    -- ^ Levene's W statistic
  , pValue     :: Double    -- ^ Associated p-value
  , rejectNull :: Bool      -- ^ Whether to reject null hypothesis at given alpha
  } deriving (Eq, Show)

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
levenesTest :: Double       -- ^ Significance level (alpha)
            -> Center       -- ^ Center calculation method
            -> [V.Vector Double] -- ^ Input samples
            -> Either String TestResult
levenesTest alpha center samples
  | alpha < 0 || alpha > 1 = Left "Significance level must be between 0 and 1"
  | length samples < 2 = Left "At least two samples required"
  | otherwise = do
      processed <- mapM processSample samples
      let (deviations, ni) = unzip processed
          k = length samples
          n = sum ni
          zbari = map Sample.mean deviations
          zbar = sum (zipWith (*) zbari (map fromIntegral ni)) / fromIntegral n

          numerator = sum [fromIntegral (ni !! i) * (zbari !! i - zbar) ** 2 | i <- [0..k-1]]
          denominator = sum [U.sum (U.map (\x -> (x - zbari !! i) ** 2) (deviations !! i)) | i <- [0..k-1]]
          
      -- Handle division by zero and invalid values
      if denominator <= 0 || isNaN denominator || isInfinite denominator
        then Left "Invalid denominator in W-statistic calculation"
        else do
          let wStat = (fromIntegral (n - k) / fromIntegral (k - 1)) * (numerator / denominator)
              df1 = k - 1
              df2 = n - k
              pVal = 1 - cumulative (fDistribution df1 df2) wStat
              reject = pVal < alpha

          -- Validate distribution parameters
          if df1 < 1 || df2 < 1
            then Left "Invalid degrees of freedom"
            else Right $ TestResult wStat pVal reject
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
        trimmed <- trimboth vec p
        let c = Sample.mean trimmed
            dev = V.map (abs . subtract c) trimmed
        return (U.convert dev, V.length trimmed)


-- Example usage:
-- import qualified Data.Vector as V
-- import LevenesTest (Center(..), levenesTest, TestResult(..))

-- main :: IO ()
-- main = do
--     let a = V.fromList [8.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
--         b = V.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 8.36, 9.18, 8.67, 9.05]
--         c = V.fromList [8.95, 9.12, 8.95, 8.85, 9.03, 8.84, 9.07, 8.98, 8.86, 8.98]

--     case levenesTest 0.05 (Trimmed 0.1) [a, b, c] of
--         Left err -> putStrLn $ "Error: " ++ err
--         Right test -> do
--             putStrLn $ "Levene's W Statistic: " ++ show (wStatistic test)
--             putStrLn $ "P-Value: " ++ show (pValue test)
--             putStrLn $ "Reject null hypothesis at α=0.05: " ++ show (rejectNull test)

-- Sample Output
-- Levene's W Statistic: 0.852
-- P-Value: 0.432
-- Reject null hypothesis at α=0.05: False
