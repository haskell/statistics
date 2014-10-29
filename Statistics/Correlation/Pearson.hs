{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Statistics.Correlation.Pearson
--

module Statistics.Correlation.Pearson
    ( pearson
    , pearsonMatByRow

    -- * References
    -- $references
    ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Statistics.Matrix
import Statistics.Sample

-- | use Welford's one-pass algorithm to calculate the Pearson's correlation 
-- between two samples.
pearson :: G.Vector v Double => v Double -> v Double -> Double
pearson x y | n /= G.length y = error errMsg
            | n <= 1 = 0/0
            | otherwise = sum_cross / (sqrt sum_xsq * sqrt sum_ysq)
  where
    (sum_cross, sum_xsq, sum_ysq) = loop (G.head x, G.head y, 0, 0, 0) 1
    loop (!mx, !my, !vx, !vy, !xy) i 
      | i >= n = (xy, vx, vy)
      | otherwise = loop ( mx + delta_x / fromIntegral (i+1)
                         , my + delta_y / fromIntegral (i+1)
                         , vx + delta_x * delta_x * ratio
                         , vy + delta_y * delta_y * ratio
                         , xy + delta_x * delta_y * ratio
                         ) (i+1)
      where
        delta_x = x G.! i - mx
        delta_y = y G.! i - my
        ratio = fromIntegral i / fromIntegral (i+1)
    n = G.length x
    errMsg = "Statistics.Correlation.Pearson.pearson: Incompatible dimensions"
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
                       | i < j -> return $ pearson vx $ m `row` j
                       | otherwise -> undefined
              UM.unsafeWrite v (i*n+j) r
              loopC (j+1)
        vx = m `row` i
    n = rows m
{-# INLINE pearsonMatByRow #-}

-- calculate pearson correlation by definition, for testing purpose
pearson' :: G.Vector v Double => v Double -> v Double -> Double
pearson' x y | n /= G.length y = error errMsg
             | n <= 1 = 0/0
             | otherwise = cov / sqrt (var_x * var_y)
  where
    m_x = mean x
    m_y = mean y
    (cov, var_x, var_y) = loop (0, 0, 0) 0
    loop (!co, !vx, !vy) i | i >= n = (co, vx, vy)
                           | otherwise = let t1 = x G.! i - m_x
                                             t2 = y G.! i - m_y
                                         in loop ( co + t1 * t2
                                                 , vx + t1 * t1
                                                 , vy + t2 * t2
                                                 ) (i+1)
    n = G.length x
    errMsg = "Statistics.Correlation.Pearson.pearson: Incompatible dimensions"
{-# INLINE pearson' #-}

-- $references
--
-- * B. P. Welford. (1962) Note on a Method for Calculating Corrected Sums of 
-- Squares and Products. /Technometrics/, Vol 4, No 3, 1962.
