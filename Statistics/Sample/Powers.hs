{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric,
    FlexibleContexts #-}
-- |
-- Module    : Statistics.Sample.Powers
-- Copyright : (c) 2009, 2010 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Very fast statistics over simple powers of a sample.  These can all
-- be computed efficiently in just a single pass over a sample, with
-- that pass subject to stream fusion.
--
-- The tradeoff is that some of these functions are less numerically
-- robust than their counterparts in the 'Statistics.Sample' module.
-- Where this is the case, the alternatives are noted.

module Statistics.Sample.Powers
    (
    -- * Types
      Powers

    -- * Constructor
    , powers

    -- * Descriptive functions
    , order
    , count
    , sum

    -- * Statistics of location
    , mean

    -- * Statistics of dispersion
    , variance
    , stdDev
    , varianceUnbiased

    -- * Functions over central moments
    , centralMoment
    , skewness
    , kurtosis

    -- * References
    -- $references
    ) where

import Data.Binary (Binary(..))
import Data.Data (Data, Typeable)
import Data.Vector.Binary ()
import Data.Vector.Generic   (unsafeFreeze)
import Data.Vector.Unboxed   ((!))
import GHC.Generics (Generic)
import Prelude hiding (sum)
import Statistics.Function   (indexed)
import Statistics.Internal   (inlinePerformIO)
import Numeric.SpecFunctions (choose)
import System.IO.Unsafe      (unsafePerformIO)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Mutable as MU

newtype Powers = Powers (U.Vector Double)
    deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary Powers

-- | O(/n/) Collect the /n/ simple powers of a sample.
--
-- Functions computed over a sample's simple powers require at least a
-- certain number (or /order/) of powers to be collected.
--
-- * To compute the /k/th 'centralMoment', at least /k/ simple powers
--   must be collected.
--
-- * For the 'variance', at least 2 simple powers are needed.
--
-- * For 'skewness', we need at least 3 simple powers.
--
-- * For 'kurtosis', at least 4 simple powers are required.
--
-- This function is subject to stream fusion.
powers :: G.Vector v Double =>
          Int                   -- ^ /n/, the number of powers, where /n/ >= 2.
       -> v Double
       -> Powers
powers k
    | k < 2     = error "Statistics.Sample.powers: too few powers"
    | otherwise = fini . G.foldl' go (unsafePerformIO $ MU.replicate l 0)
  where
    go ms x = inlinePerformIO $ loop 0 1
        where loop !i !xk | i == l = return ms
                          | otherwise = do
                MU.read ms i >>= MU.write ms i . (+ xk)
                loop (i+1) (xk*x)
    fini = Powers . unsafePerformIO . unsafeFreeze
    l    = k + 1
{-# INLINE powers #-}

-- | The order (number) of simple powers collected from a 'sample'.
order :: Powers -> Int
order (Powers pa) = U.length pa - 1
{-# INLINE order #-}

-- | Compute the /k/th central moment of a sample.  The central
-- moment is also known as the moment about the mean.
centralMoment :: Int -> Powers -> Double
centralMoment k p@(Powers pa)
    | k < 0 || k > order p =
                  error ("Statistics.Sample.Powers.centralMoment: "
                         ++ "invalid argument")
    | k == 0    = 1
    | otherwise = (/n) . U.sum . U.map go . indexed . U.take (k+1) $ pa
  where
    go (i , e) = (k `choose` i) * ((-m) ^ (k-i)) * e
    n = U.head pa
    m = mean p
{-# INLINE centralMoment #-}

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance, where the denominator is /n/.  This is
-- the second central moment of the sample.
--
-- This is less numerically robust than the variance function in the
-- 'Statistics.Sample' module, but the number is essentially free to
-- compute if you have already collected a sample's simple powers.
--
-- Requires 'Powers' with 'order' at least 2.
variance :: Powers -> Double
variance = centralMoment 2
{-# INLINE variance #-}

-- | Standard deviation.  This is simply the square root of the
-- maximum likelihood estimate of the variance.
stdDev :: Powers -> Double
stdDev = sqrt . variance
{-# INLINE stdDev #-}

-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance, where the denominator is /n/-1.
--
-- Requires 'Powers' with 'order' at least 2.
varianceUnbiased :: Powers -> Double
varianceUnbiased p@(Powers pa)
    | n > 1     = variance p * n / (n-1)
    | otherwise = 0
  where n = U.head pa
{-# INLINE varianceUnbiased #-}

-- | Compute the skewness of a sample. This is a measure of the
-- asymmetry of its distribution.
--
-- A sample with negative skew is said to be /left-skewed/.  Most of
-- its mass is on the right of the distribution, with the tail on the
-- left.
--
-- > skewness . powers 3 $ U.to [1,100,101,102,103]
-- > ==> -1.497681449918257
--
-- A sample with positive skew is said to be /right-skewed/.
--
-- > skewness . powers 3 $ U.to [1,2,3,4,100]
-- > ==> 1.4975367033335198
--
-- A sample's skewness is not defined if its 'variance' is zero.
--
-- Requires 'Powers' with 'order' at least 3.
skewness :: Powers -> Double
skewness p = centralMoment 3 p * variance p ** (-1.5)
{-# INLINE skewness #-}

-- | Compute the excess kurtosis of a sample.  This is a measure of
-- the \"peakedness\" of its distribution.  A high kurtosis indicates
-- that the sample's variance is due more to infrequent severe
-- deviations than to frequent modest deviations.
--
-- A sample's excess kurtosis is not defined if its 'variance' is
-- zero.
--
-- Requires 'Powers' with 'order' at least 4.
kurtosis :: Powers -> Double
kurtosis p = centralMoment 4 p / (v * v) - 3
    where v = variance p
{-# INLINE kurtosis #-}

-- | The number of elements in the original 'Sample'.  This is the
-- sample's zeroth simple power.
count :: Powers -> Int
count (Powers pa) = floor $ U.head pa
{-# INLINE count #-}

-- | The sum of elements in the original 'Sample'.  This is the
-- sample's first simple power.
sum :: Powers -> Double
sum (Powers pa) = pa ! 1
{-# INLINE sum #-}

-- | The arithmetic mean of elements in the original 'Sample'.
--
-- This is less numerically robust than the mean function in the
-- 'Statistics.Sample' module, but the number is essentially free to
-- compute if you have already collected a sample's simple powers.
mean :: Powers -> Double
mean p@(Powers pa)
    | n == 0    = 0
    | otherwise = sum p / n
    where n     = U.head pa
{-# INLINE mean #-}

-- $references
--
-- * Besset, D.H. (2000) Elements of statistics.
--   /Object-oriented implementation of numerical methods/
--   ch. 9, pp. 311&#8211;331.
--   <http://www.elsevier.com/wps/product/cws_home/677916>
--
-- * Anderson, G. (2009) Compute /k/th central moments in one
--   pass. /quantblog/. <http://quantblog.wordpress.com/2009/02/07/compute-kth-central-moments-in-one-pass/>
