{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Statistics.Correlation.Pearson
--

module Statistics.Correlation.Pearson
    ( pearson
    , pearsonMatByRow
    ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Statistics.Matrix
import Statistics.Sample

-- | Pearson correlation for sample of pairs.
pearson :: (G.Vector v (Double, Double), G.Vector v Double)
        => v (Double, Double) -> Double
pearson = correlation
{-# INLINE pearson #-}

-- | compute pairwise pearson correlation between rows of a matrix
pearsonMatByRow :: Matrix -> Matrix
pearsonMatByRow m = fromVector n n $ U.create $ UM.replicate (n*n) 1.0 >>= loopR 0
  where
    loopR !i v | i >= n = return v
               | otherwise = loopC 0 >> loopR (i+1) v
      where 
        loopC !j 
          | j >= n = return ()
          | otherwise = do
              r <- case () of
                     _ | i == j -> return 1
                       | i > j -> UM.unsafeRead v (j*n+i)
                       | i < j -> return . pearson . G.zip vx $ m `row` j
                       | otherwise -> undefined
              UM.unsafeWrite v (i*n+j) r
              loopC (j+1)
        vx = m `row` i
    n = rows m
{-# INLINE pearsonMatByRow #-}
