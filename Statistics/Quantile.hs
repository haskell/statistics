{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Quantile
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for approximating quantiles, i.e. points taken at regular
-- intervals from the cumulative distribution function of a random
-- variable.
--
-- The number of quantiles is described below by the variable /q/, so
-- with /q/=4, a 4-quantile (also known as a /quartile/) has 4
-- intervals, and contains 5 points.  The parameter /k/ describes the
-- desired point, where 0 &#8804; /k/ &#8804; /q/.

module Statistics.Quantile
    (
    -- * Quantile estimation functions
      weightedAvg
    , ContParam(..)
    , continuousBy
    , midspread

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

import Data.Vector.Generic             ((!))
import Numeric.MathFunctions.Constants (m_epsilon)
import Statistics.Function             (partialSort)
import qualified Data.Vector.Generic as G

-- | O(/n/ log /n/). Estimate the /k/th /q/-quantile of a sample,
-- using the weighted average method.
weightedAvg :: G.Vector v Double => 
               Int        -- ^ /k/, the desired quantile.
            -> Int        -- ^ /q/, the number of quantiles.
            -> v Double   -- ^ /x/, the sample data.
            -> Double
weightedAvg k q x
  | G.any isNaN x   = modErr "weightedAvg" "Sample contains NaNs"
  | n == 1          = G.head x
  | q < 2           = modErr "weightedAvg" "At least 2 quantiles is needed"
  | k < 0 || k >= q = modErr "weightedAvg" "Wrong quantile number"
  | otherwise       = xj + g * (xj1 - xj)
  where
    j   = floor idx
    idx = fromIntegral (n - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    xj  = sx ! j
    xj1 = sx ! (j+1)
    sx  = partialSort (j+2) x
    n   = G.length x
{-# INLINE weightedAvg #-}

-- | Parameters /a/ and /b/ to the 'continuousBy' function.
data ContParam = ContParam {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | O(/n/ log /n/). Estimate the /k/th /q/-quantile of a sample /x/,
-- using the continuous sample method with the given parameters.  This
-- is the method used by most statistical software, such as R,
-- Mathematica, SPSS, and S.
continuousBy :: G.Vector v Double =>
                ContParam  -- ^ Parameters /a/ and /b/.
             -> Int        -- ^ /k/, the desired quantile.
             -> Int        -- ^ /q/, the number of quantiles.
             -> v Double   -- ^ /x/, the sample data.
             -> Double
continuousBy (ContParam a b) k q x
  | q < 2          = modErr "continuousBy" "At least 2 quantiles is needed"
  | k < 0 || k > q = modErr "continuousBy" "Wrong quantile number"
  | G.any isNaN x  = modErr "continuousBy" "Sample contains NaNs"
  | otherwise      = (1-h) * item (j-1) + h * item j
  where
    j               = floor (t + eps)
    t               = a + p * (fromIntegral n + 1 - a - b)
    p               = fromIntegral k / fromIntegral q
    h | abs r < eps = 0
      | otherwise   = r
      where r       = t - fromIntegral j
    eps             = m_epsilon * 4
    n               = G.length x
    item            = (sx !) . bracket
    sx              = partialSort (bracket j + 1) x
    bracket m       = min (max m 0) (n - 1)
{-# INLINE continuousBy #-}

-- | O(/n/ log /n/). Estimate the range between /q/-quantiles 1 and
-- /q/-1 of a sample /x/, using the continuous sample method with the
-- given parameters.
--
-- For instance, the interquartile range (IQR) can be estimated as
-- follows:
--
-- > midspread medianUnbiased 4 (U.fromList [1,1,2,2,3])
-- > ==> 1.333333
midspread :: G.Vector v Double =>
             ContParam  -- ^ Parameters /a/ and /b/.
          -> Int        -- ^ /q/, the number of quantiles.
          -> v Double   -- ^ /x/, the sample data.
          -> Double
midspread (ContParam a b) k x
  | G.any isNaN x = modErr "midspread" "Sample contains NaNs"
  | k <= 0        = modErr "midspread" "Nonpositive number of quantiles"
  | otherwise     = quantile (1-frac) - quantile frac
  where
    quantile i        = (1-h i) * item (j i-1) + h i * item (j i)
    j i               = floor (t i + eps) :: Int
    t i               = a + i * (fromIntegral n + 1 - a - b)
    h i | abs r < eps = 0
        | otherwise   = r
        where r       = t i - fromIntegral (j i)
    eps               = m_epsilon * 4
    n                 = G.length x
    item              = (sx !) . bracket
    sx                = partialSort (bracket (j (1-frac)) + 1) x
    bracket m         = min (max m 0) (n - 1)
    frac              = 1 / fromIntegral k
{-# INLINE midspread #-}

-- | California Department of Public Works definition, /a/=0, /b/=1.
-- Gives a linear interpolation of the empirical CDF.  This
-- corresponds to method 4 in R and Mathematica.
cadpw :: ContParam
cadpw = ContParam 0 1
{-# INLINE cadpw #-}

-- | Hazen's definition, /a/=0.5, /b/=0.5.  This is claimed to be
-- popular among hydrologists.  This corresponds to method 5 in R and
-- Mathematica.
hazen :: ContParam
hazen = ContParam 0.5 0.5
{-# INLINE hazen #-}

-- | Definition used by the SPSS statistics application, with /a/=0,
-- /b/=0 (also known as Weibull's definition).  This corresponds to
-- method 6 in R and Mathematica.
spss :: ContParam
spss = ContParam 0 0
{-# INLINE spss #-}

-- | Definition used by the S statistics application, with /a/=1,
-- /b/=1.  The interpolation points divide the sample range into @n-1@
-- intervals.  This corresponds to method 7 in R and Mathematica.
s :: ContParam
s = ContParam 1 1
{-# INLINE s #-}

-- | Median unbiased definition, /a/=1\/3, /b/=1\/3. The resulting
-- quantile estimates are approximately median unbiased regardless of
-- the distribution of /x/.  This corresponds to method 8 in R and
-- Mathematica.
medianUnbiased :: ContParam
medianUnbiased = ContParam third third
    where third = 1/3
{-# INLINE medianUnbiased #-}

-- | Normal unbiased definition, /a/=3\/8, /b/=3\/8.  An approximately
-- unbiased estimate if the empirical distribution approximates the
-- normal distribution.  This corresponds to method 9 in R and
-- Mathematica.
normalUnbiased :: ContParam
normalUnbiased = ContParam ta ta
    where ta = 3/8
{-# INLINE normalUnbiased #-}

modErr :: String -> String -> a
modErr f err = error $ "Statistics.Quantile." ++ f ++ ": " ++ err

-- $references
--
-- * Weisstein, E.W. Quantile. /MathWorld/.
--   <http://mathworld.wolfram.com/Quantile.html>
--
-- * Hyndman, R.J.; Fan, Y. (1996) Sample quantiles in statistical
--   packages. /American Statistician/
--   50(4):361&#8211;365. <http://www.jstor.org/stable/2684934>
