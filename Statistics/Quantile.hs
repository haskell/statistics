{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ViewPatterns       #-}
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
-- desired point, where 0 ≤ /k/ ≤ /q/.

module Statistics.Quantile
    (
    -- * Quantile estimation functions
    -- $cont_quantiles
      ContParam(..)
    , Default(..)
    , quantile
    , quantiles
    , quantilesVec
    -- ** Parameters for the continuous sample method
    , cadpw
    , hazen
    , spss
    , s
    , medianUnbiased
    , normalUnbiased
    -- * Other algorithms
    , weightedAvg
    -- * Median & other specializations
    , median
    , mad
    , midspread
    -- * Deprecated
    , continuousBy
    -- * References
    -- $references
    ) where

import           Data.Binary            (Binary)
import           Data.Aeson             (ToJSON,FromJSON)
import           Data.Data              (Data,Typeable)
import           Data.Default.Class
import qualified Data.Foldable        as F
import           Data.Vector.Generic ((!))
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S
import GHC.Generics (Generic)

import Statistics.Function (partialSort)


----------------------------------------------------------------
-- Quantile estimation
----------------------------------------------------------------

-- | O(/n/·log /n/). Estimate the /k/th /q/-quantile of a sample,
-- using the weighted average method. Up to rounding errors it's same
-- as @quantile s@.
--
-- The following properties should hold otherwise an error will be thrown.
--
--   * the length of the input is greater than @0@
--
--   * the input does not contain @NaN@
--
--   * k ≥ 0 and k ≤ q
weightedAvg :: G.Vector v Double =>
               Int        -- ^ /k/, the desired quantile.
            -> Int        -- ^ /q/, the number of quantiles.
            -> v Double   -- ^ /x/, the sample data.
            -> Double
weightedAvg k q x
  | G.any isNaN x   = modErr "weightedAvg" "Sample contains NaNs"
  | n == 0          = modErr "weightedAvg" "Sample is empty"
  | n == 1          = G.head x
  | q < 2           = modErr "weightedAvg" "At least 2 quantiles is needed"
  | k == q          = G.maximum x
  | k >= 0 || k < q = xj + g * (xj1 - xj)
  | otherwise       = modErr "weightedAvg" "Wrong quantile number"
  where
    j   = floor idx
    idx = fromIntegral (n - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    xj  = sx ! j
    xj1 = sx ! (j+1)
    sx  = partialSort (j+2) x
    n   = G.length x
{-# SPECIALIZE weightedAvg :: Int -> Int -> U.Vector Double -> Double #-}
{-# SPECIALIZE weightedAvg :: Int -> Int -> V.Vector Double -> Double #-}
{-# SPECIALIZE weightedAvg :: Int -> Int -> S.Vector Double -> Double #-}


----------------------------------------------------------------
-- Quantiles continuous algorithm
----------------------------------------------------------------

-- $cont_quantiles
--
-- Below is family of functions which use same algorithm for estimation
-- of sample quantiles. It approximates empirical CDF as continuous
-- piecewise function which interpolates linearly between points
-- \((X_k,p_k)\) where \(X_k\) is k-th order statistics (k-th smallest
-- element) and \(p_k\) is probability corresponding to
-- it. 'ContParam' determines how \(p_k\) is chosen. For more detailed
-- explanation see [Hyndman1996].
--
-- This is the method used by most statistical software, such as R,
-- Mathematica, SPSS, and S.


-- | Parameters /α/ and /β/ to the 'continuousBy' function. Exact
--   meaning of parameters is described in [Hyndman1996] in section
--   \"Piecewise linear functions\"
data ContParam = ContParam {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

-- | We use 's' as default value which is same as R's default.
instance Default ContParam where
  def = s

instance Binary   ContParam
instance ToJSON   ContParam
instance FromJSON ContParam

-- | O(/n/·log /n/). Estimate the /k/th /q/-quantile of a sample /x/,
--   using the continuous sample method with the given parameters.
--
--   The following properties should hold, otherwise an error will be thrown.
--
--     * input sample must be nonempty
--
--     * the input does not contain @NaN@
--
--     * 0 ≤ k ≤ q
quantile :: G.Vector v Double
         => ContParam  -- ^ Parameters /α/ and /β/.
         -> Int        -- ^ /k/, the desired quantile.
         -> Int        -- ^ /q/, the number of quantiles.
         -> v Double   -- ^ /x/, the sample data.
         -> Double
quantile param q nQ xs
  | nQ < 2         = modErr "continuousBy" "At least 2 quantiles is needed"
  | badQ nQ q      = modErr "continuousBy" "Wrong quantile number"
  | G.any isNaN xs = modErr "continuousBy" "Sample contains NaNs"
  | otherwise      = estimateQuantile sortedXs pk
  where
    pk       = toPk param n q nQ
    sortedXs = psort xs $ floor pk + 1
    n        = G.length xs
{-# INLINABLE quantile #-}
{-# SPECIALIZE
    quantile :: ContParam -> Int -> Int -> U.Vector Double -> Double #-}
{-# SPECIALIZE
    quantile :: ContParam -> Int -> Int -> V.Vector Double -> Double #-}
{-# SPECIALIZE
    quantile :: ContParam -> Int -> Int -> S.Vector Double -> Double #-}

-- | O(/k·n/·log /n/). Estimate set of the /k/th /q/-quantile of a
--   sample /x/, using the continuous sample method with the given
--   parameters. This is faster than calling quantile repeatedly since
--   sample should be sorted only once
--
--   The following properties should hold, otherwise an error will be thrown.
--
--     * input sample must be nonempty
--
--     * the input does not contain @NaN@
--
--     * for every k in set of quantiles 0 ≤ k ≤ q
quantiles :: (G.Vector v Double, F.Foldable f, Functor f)
  => ContParam
  -> f Int
  -> Int
  -> v Double
  -> f Double
quantiles param qs nQ xs
  | nQ < 2             = modErr "quantiles" "At least 2 quantiles is needed"
  | F.any (badQ nQ) qs = modErr "quantiles" "Wrong quantile number"
  | G.any isNaN xs     = modErr "quantiles" "Sample contains NaNs"
  -- Doesn't matter what we put into empty container
  | null qs            = 0 <$ qs
  | otherwise          = fmap (estimateQuantile sortedXs) ks'
  where
    ks'      = fmap (\q -> toPk param n q nQ) qs
    sortedXs = psort xs $ floor (F.maximum ks') + 1
    n        = G.length xs
{-# INLINABLE quantiles #-}
{-# SPECIALIZE quantiles
      :: (Functor f, F.Foldable f) => ContParam -> f Int -> Int -> V.Vector Double -> f Double #-}
{-# SPECIALIZE quantiles
      :: (Functor f, F.Foldable f) => ContParam -> f Int -> Int -> U.Vector Double -> f Double #-}
{-# SPECIALIZE quantiles
      :: (Functor f, F.Foldable f) => ContParam -> f Int -> Int -> S.Vector Double -> f Double #-}

-- | O(/k·n/·log /n/). Same as quantiles but uses 'G.Vector' container
--   instead of 'Foldable' one.
quantilesVec :: (G.Vector v Double, G.Vector v Int)
  => ContParam
  -> v Int
  -> Int
  -> v Double
  -> v Double
quantilesVec param qs nQ xs
  | nQ < 2             = modErr "quantilesVec" "At least 2 quantiles is needed"
  | G.any (badQ nQ) qs = modErr "quantilesVec" "Wrong quantile number"
  | G.any isNaN xs     = modErr "quantilesVec" "Sample contains NaNs"
  | G.null qs          = G.empty
  | otherwise          = G.map (estimateQuantile sortedXs) ks'
  where
    ks'      = G.map (\q -> toPk param n q nQ) qs
    sortedXs = psort xs $ floor (G.maximum ks') + 1
    n        = G.length xs
{-# INLINABLE quantilesVec #-}
{-# SPECIALIZE quantilesVec
      :: ContParam -> V.Vector Int -> Int -> V.Vector Double -> V.Vector Double #-}
{-# SPECIALIZE quantilesVec
      :: ContParam -> U.Vector Int -> Int -> U.Vector Double -> U.Vector Double #-}
{-# SPECIALIZE quantilesVec
      :: ContParam -> S.Vector Int -> Int -> S.Vector Double -> S.Vector Double #-}


-- Returns True if quantile number is out of range
badQ :: Int -> Int -> Bool
badQ nQ q = q < 0 || q > nQ

-- Obtain k from equation for p_k [Hyndman1996] p.363.  Note that
-- equation defines p_k for integer k but we calculate it as real
-- value and will use fractional part for linear interpolation. This
-- is correct since equation is linear.
toPk
  :: ContParam
  -> Int        -- ^ /n/ number of elements
  -> Int        -- ^ /k/, the desired quantile.
  -> Int        -- ^ /q/, the number of quantiles.
  -> Double
toPk (ContParam a b) (fromIntegral -> n) q nQ
  = a + p * (n + 1 - a - b)
  where
    p = fromIntegral q / fromIntegral nQ

-- Estimate quantile for given k (including fractional part)
estimateQuantile :: G.Vector v Double => v Double -> Double -> Double
{-# INLINE estimateQuantile #-}
estimateQuantile sortedXs k'
  = (1-g) * item (k-1) + g * item k
  where
    (k,g) = properFraction k'
    item  = (sortedXs !) . clamp
    --
    clamp = max 0 . min (n - 1)
    n     = G.length sortedXs

psort :: G.Vector v Double => v Double -> Int -> v Double
psort xs k = partialSort (max 0 $ min (G.length xs - 1) k) xs
{-# INLINE psort #-}


-- | California Department of Public Works definition, /α/=0, /β/=1.
-- Gives a linear interpolation of the empirical CDF.  This
-- corresponds to method 4 in R and Mathematica.
cadpw :: ContParam
cadpw = ContParam 0 1

-- | Hazen's definition, /α/=0.5, /β/=0.5.  This is claimed to be
-- popular among hydrologists.  This corresponds to method 5 in R and
-- Mathematica.
hazen :: ContParam
hazen = ContParam 0.5 0.5

-- | Definition used by the SPSS statistics application, with /α/=0,
-- /β/=0 (also known as Weibull's definition).  This corresponds to
-- method 6 in R and Mathematica.
spss :: ContParam
spss = ContParam 0 0

-- | Definition used by the S statistics application, with /α/=1,
-- /β/=1.  The interpolation points divide the sample range into @n-1@
-- intervals.  This corresponds to method 7 in R and Mathematica and
-- is default in R.
s :: ContParam
s = ContParam 1 1

-- | Median unbiased definition, /α/=1\/3, /β/=1\/3. The resulting
-- quantile estimates are approximately median unbiased regardless of
-- the distribution of /x/.  This corresponds to method 8 in R and
-- Mathematica.
medianUnbiased :: ContParam
medianUnbiased = ContParam third third
    where third = 1/3

-- | Normal unbiased definition, /α/=3\/8, /β/=3\/8.  An approximately
-- unbiased estimate if the empirical distribution approximates the
-- normal distribution.  This corresponds to method 9 in R and
-- Mathematica.
normalUnbiased :: ContParam
normalUnbiased = ContParam ta ta
    where ta = 3/8

modErr :: String -> String -> a
modErr f err = error $ "Statistics.Quantile." ++ f ++ ": " ++ err


----------------------------------------------------------------
-- Specializations
----------------------------------------------------------------

-- | O(/n/·log /n/) Estimate median of sample
median :: G.Vector v Double
       => ContParam  -- ^ Parameters /α/ and /β/.
       -> v Double   -- ^ /x/, the sample data.
       -> Double
{-# INLINE median #-}
median p = quantile p 1 2

-- | O(/n/·log /n/). Estimate the range between /q/-quantiles 1 and
-- /q/-1 of a sample /x/, using the continuous sample method with the
-- given parameters.
--
-- For instance, the interquartile range (IQR) can be estimated as
-- follows:
--
-- > midspread medianUnbiased 4 (U.fromList [1,1,2,2,3])
-- > ==> 1.333333
midspread :: G.Vector v Double =>
             ContParam  -- ^ Parameters /α/ and /β/.
          -> Int        -- ^ /q/, the number of quantiles.
          -> v Double   -- ^ /x/, the sample data.
          -> Double
midspread param k x
  | G.any isNaN x = modErr "midspread" "Sample contains NaNs"
  | k <= 0        = modErr "midspread" "Nonpositive number of quantiles"
  | otherwise     = let Pair x1 x2 = quantiles param (Pair 1 (k-1)) k x
                    in  x2 - x1
{-# INLINABLE  midspread #-}
{-# SPECIALIZE midspread :: ContParam -> Int -> U.Vector Double -> Double #-}
{-# SPECIALIZE midspread :: ContParam -> Int -> V.Vector Double -> Double #-}
{-# SPECIALIZE midspread :: ContParam -> Int -> S.Vector Double -> Double #-}

data Pair a = Pair !a !a
  deriving (Functor, F.Foldable)


-- | O(/n/·log /n/). Estimate the median absolute deviation (MAD) of a
--   sample /x/ using 'continuousBy'. It's robust estimate of
--   variability in sample and defined as:
--
--   \[
--   MAD = \operatorname{median}(| X_i - \operatorname{median}(X) |)
--   \]
mad :: G.Vector v Double
    => ContParam  -- ^ Parameters /α/ and /β/.
    -> v Double   -- ^ /x/, the sample data.
    -> Double
mad p xs
  = median p $ G.map (abs . subtract med) xs
  where
    med = median p xs
{-# INLINABLE  mad #-}
{-# SPECIALIZE mad :: ContParam -> U.Vector Double -> Double #-}
{-# SPECIALIZE mad :: ContParam -> V.Vector Double -> Double #-}
{-# SPECIALIZE mad :: ContParam -> S.Vector Double -> Double #-}


----------------------------------------------------------------
-- Deprecated
----------------------------------------------------------------

continuousBy :: G.Vector v Double =>
                ContParam  -- ^ Parameters /α/ and /β/.
             -> Int        -- ^ /k/, the desired quantile.
             -> Int        -- ^ /q/, the number of quantiles.
             -> v Double   -- ^ /x/, the sample data.
             -> Double
continuousBy = quantile
{-# DEPRECATED continuousBy "Use quantile instead" #-}

-- $references
--
-- * Weisstein, E.W. Quantile. /MathWorld/.
--   <http://mathworld.wolfram.com/Quantile.html>
--
-- * [Hyndman1996] Hyndman, R.J.; Fan, Y. (1996) Sample quantiles in statistical
--   packages. /American Statistician/
--   50(4):361&#8211;365. <http://www.jstor.org/stable/2684934>
