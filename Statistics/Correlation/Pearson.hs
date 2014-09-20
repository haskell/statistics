{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Statistics.Correlation.Pearson
--

module Statistics.Correlation.Pearson
    ( pearson
    , pearson'

    -- * References
    -- $references
    ) where

import Statistics.Sample
import qualified Data.Vector.Generic as G

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
