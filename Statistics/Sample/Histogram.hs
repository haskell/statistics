{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Statistics.Sample.Histogram
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for computing histograms of sample data.

module Statistics.Sample.Histogram
    (
      histogram
    -- * Building blocks
    , histogram_
    , range
    ) where

import Numeric.MathFunctions.Constants (m_epsilon)
import Statistics.Function (minMax)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

-- | /O(n)/ Compute a histogram over a data set.
--
-- The result consists of a pair of vectors:
--
-- * The lower bound of each interval.
--
-- * The number of samples within the interval.
--
-- Interval (bin) sizes are uniform, and the upper and lower bounds
-- are chosen automatically using the 'range' function.  To specify
-- these parameters directly, use the 'histogram_' function.
histogram :: (G.Vector v0 Double, G.Vector v1 Double, Num b, G.Vector v1 b) =>
             Int                -- ^ Number of bins (must be positive).
          -> v0 Double          -- ^ Sample data (cannot be empty).
          -> (v1 Double, v1 b)
histogram numBins xs = (G.generate numBins step, histogram_ numBins lo hi xs)
    where (lo,hi)    = range numBins xs
          step i     = lo + d * fromIntegral i
          d          = (hi - lo) / fromIntegral numBins
{-# INLINE histogram #-}

-- | /O(n)/ Compute a histogram over a data set.
--
-- Interval (bin) sizes are uniform, based on the supplied upper
-- and lower bounds.
histogram_ :: (Num b, RealFrac a, G.Vector v0 a, G.Vector v1 b) =>
              Int
           -- ^ Number of bins.  This value must be positive.  A zero
           -- or negative value will cause an error.
           -> a
           -- ^ Lower bound on interval range.  Sample data less than
           -- this will cause an error.
           -> a
           -- ^ Upper bound on interval range.  This value must not be
           -- less than the lower bound.  Sample data that falls above
           -- the upper bound will cause an error.
           -> v0 a
           -- ^ Sample data.
           -> v1 b
histogram_ numBins lo hi xs0 = G.create (GM.replicate numBins 0 >>= bin xs0)
  where
    bin xs bins = go 0
     where
       go i | i >= len = return bins
            | otherwise = do
         let x = xs `G.unsafeIndex` i
             b = truncate $ (x - lo) / d
         GM.write bins b . (+1) =<< GM.read bins b
         go (i+1)
       len = G.length xs
       d = ((hi - lo) * (1 + realToFrac m_epsilon)) / fromIntegral numBins
{-# INLINE histogram_ #-}

-- | /O(n)/ Compute decent defaults for the lower and upper bounds of
-- a histogram, based on the desired number of bins and the range of
-- the sample data.
--
-- The upper and lower bounds used are @(lo-d, hi+d)@, where
--
-- @d = (maximum sample - minimum sample) / ((bins - 1) * 2)@
range :: (G.Vector v Double) =>
         Int                    -- ^ Number of bins (must be positive).
      -> v Double               -- ^ Sample data (cannot be empty).
             -> (Double, Double)
range numBins xs
    | numBins < 1 = error "Statistics.Histogram.range: invalid bin count"
    | G.null xs   = error "Statistics.Histogram.range: empty sample"
    | otherwise   = (lo-d, hi+d)
  where
    d | numBins == 1 = 0
      | otherwise    = (hi - lo) / ((fromIntegral numBins - 1) * 2)
    (lo,hi)          = minMax xs
{-# INLINE range #-}
