{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Goodness of fit runs tests.
module Statistics.Test.Runs (
    runsTest
  , module Statistics.Test.Types
  ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Numeric.SpecFunctions (choose)
import Prelude hiding (sum)

import Statistics.Sample.Internal (sum)
import Statistics.Test.Types
import Statistics.Types


-- | Goodness of fit test for binned data. It uses only sign of
--   deviations of observations from their expectations. Null
--   hypothesis is that all possible patterns of sign occurrences are
--   equiprobable.
--
--   It's asymptotically independent from chi-square test. So their
--   results could be directly combined
runsTest :: (G.Vector v Bool) => v Bool -> Test ()
{-# INLINE runsTest #-}
runsTest v
  = Test { testSignificance = mkPValue $ cumulativeProb n m r
         , testStatistics   = fromIntegral r
         , testDistribution = ()
         }
  where
    (n,m,r) = computeRuns v

-- Compute number of positive elements, negative elements and runs
computeRuns :: (G.Vector v Bool) => v Bool -> (Int,Int,Int)
{-# INLINE computeRuns #-}
computeRuns v
  = fini $ G.foldl' step (0,0,0,Nothing) v
  where
    step (!nP,!nM,!nR,!old) f =
      ( if f then nP+1 else nP
      , if f then nM   else nM+1
      , if old == Just f then nR else nR+1
      , Just f
      )
    fini (nP,nM,nR,_) = (nP,nM,nR)

-- Compute denormalized probability of getting R runs given N positive
-- and M positive elements
denormProbability :: Int -> Int -> Int -> Double
denormProbability n m r
  | even r    = 2 * ((m-1) `choose` (s-1)) * ((n-1) `choose` (s-1))
  | otherwise = ((m-1) `choose` (s-1)) * ((n-1) `choose` (s-2))
              + ((m-1) `choose` (s-2)) * ((n-1) `choose` (s-1))
  where
    s = r `quot` 2

-- Probability of getting R<=R[observed]
cumulativeProb :: Int -> Int -> Int -> Double
cumulativeProb n m r
  = min 1
  $ sum (U.map (denormProbability n m) $ U.enumFromTo 1 r)
  / ((n+m) `choose` m)
