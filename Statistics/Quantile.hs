{-# LANGUAGE TypeOperators #-}
-- |
-- Module    : Statistics.Quantile
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for approximating quantiles.

module Statistics.Quantile
    (
     -- * Types
     ContParam(..)

    -- * Quantile estimation functions
    , weightedAvg
    , continuousBy

    -- * Parameters for the continuous sample method
    , cadpw
    , hazen
    , s
    , spss
    , medianUnbiased
    , normalUnbiased

    -- * References
    -- $references
    ) where

import Control.Exception (assert)
import Data.Array.Vector (allU, indexU, lengthU)
import Statistics.Function (partialSort)
import Statistics.Types (Sample)

-- | Use the weighted average method to estimate the @k@th
-- @q@-quantile of a sample.
weightedAvg :: Int              -- ^ @k@, the desired quantile
            -> Int              -- ^ @q@, the number of quantiles
            -> Sample           -- ^ @x@, the sample data
            -> Double
weightedAvg k q x =
    assert (q >= 2) .
    assert (k >= 0) .
    assert (k < q) .
    assert (allU (not . isNaN) x) $
    xj + g * (xj1 - xj)
  where
    j   = floor idx
    idx = fromIntegral (lengthU x - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    xj  = indexU sx j
    xj1 = indexU sx (j+1)
    sx  = partialSort (j+2) x
{-# INLINE weightedAvg #-}

-- | Parameters @a@ and @b@ to the 'quantileBy' function.
data ContParam = ContParam {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Using the continuous sample method with the given parameters,
-- estimate the @k@th @q@-quantile of a sample @x@.
continuousBy :: ContParam       -- ^ Parameters @a@ and @b@
             -> Int             -- ^ @k@, the desired quantile
             -> Int             -- ^ @q@, the number of quantiles
             -> Sample          -- ^ @x@, the sample data
             -> Double
continuousBy (ContParam a b) k q x =
    assert (q >= 2) .
    assert (k >= 0) .
    assert (k <= q) .
    assert (allU (not . isNaN) x) $
    (1-h) * item (j-1) + h * item j
  where
    j               = floor (t + eps)
    t               = a + p * (fromIntegral n + 1 - a - b)
    p               = fromIntegral k / fromIntegral q
    h | abs r < eps = 0
      | otherwise   = r
      where r       = t - fromIntegral j
    eps             = 8.881784e-16
    n               = lengthU x
    item m          = indexU sx $ bracket m
    sx              = partialSort (bracket j + 1) x
    bracket m       = min (max m 0) (n - 1)
{-# INLINE continuousBy #-}

-- | California Department of Public Works definition, @a=0,b=1@.
-- Gives a linear interpolation of the empirical CDF.
-- This corresponds to method 4 in R and Mathematica.
cadpw :: ContParam
cadpw = ContParam 0 1
{-# INLINE cadpw #-}

-- | Hazen's definition, @a=0.5,b=0.5@.  This is claimed to be popular
-- among hydrologists.  This corresponds to method 5 in R and
-- Mathematica.
hazen :: ContParam
hazen = ContParam 0.5 0.5
{-# INLINE hazen #-}

-- | SPSS definition, @a=0,b=0@, also known as Weibull's definition.
-- This corresponds to method 6 in R and Mathematica.
spss :: ContParam
spss = ContParam 0 0
{-# INLINE spss #-}

-- | S definition, @a=1,b=1@.  The interpolation points divide the
-- sample range into @n-1@ intervals.  This corresponds to method 7 in
-- R and Mathematica.
s :: ContParam
s = ContParam 1 1
{-# INLINE s #-}

-- | Median unbiased definition, @a=1/3,b=1/3@. The resulting quantile
-- estimates are approximately median unbiased regardless of the
-- distribution of @x@.  This corresponds to method 8 in R and
-- Mathematica.
medianUnbiased :: ContParam
medianUnbiased = ContParam third third
    where third = 1/3
{-# INLINE medianUnbiased #-}

-- | Normal unbiased definition, @a=3/8,b=3/8@.  An approximately
-- unbiased estimate if the empirical distribution approximates the
-- normal distribution.  This corresponds to method 9 in R and
-- Mathematica.
normalUnbiased :: ContParam
normalUnbiased = ContParam ta ta
    where ta = 3/8
{-# INLINE normalUnbiased #-}

-- $references
--
-- * Weisstein, E.W. Quantile. /MathWorld/.
--   <http://mathworld.wolfram.com/Quantile.html>
--
-- * Hyndman, R.J.; Fan, Y. (1996) Sample quantiles in statistical
--   packages. /American Statistician/
--   50(4):361&#8211;365. <http://www.jstor.org/stable/2684934>

