{-# LANGUAGE DeriveDataTypeable,DeriveGeneric #-}
-- |
-- Module    : Statistics.Test.KolmogorovSmirnov
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Kolmogov-Smirnov tests are non-parametric tests for assesing
-- whether given sample could be described by distribution or whether
-- two samples have the same distribution. It's only applicable to
-- continous distributions.
module Statistics.Test.KolmogorovSmirnov (
    -- * Data types
    StatisticKS(..)
    -- * Kolmogorov-Smirnov test
  , kolmogorovSmirnovTest
  , kolmogorovSmirnovTestCdf
  , kolmogorovSmirnovTest2
    -- * Evaluate statistics
  , kolmogorovSmirnovCdfD
  , kolmogorovSmirnovD
  , kolmogorovSmirnov2D
    -- * Probablities
  , kolmogorovSmirnovProbability
    -- * References
    -- $references
  ) where

import Control.Monad (when)
import Control.DeepSeq (NFData)
import Prelude hiding (exponent, sum)
import Statistics.Distribution (Distribution(..))
import Statistics.Function (sort, unsafeModify)
import Statistics.Matrix (center, exponent, for, fromVector, power)
import Statistics.Test.Types
import Statistics.Types (Sample,confLevel)
import Data.Aeson  (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Data   (Typeable,Data)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import GHC.Generics (Generic)

----------------------------------------------------------------
-- Test
----------------------------------------------------------------

-- | Check that sample could be described by distribution.
--
--   This test uses Marsaglia-Tsang-Wang exact alogorithm for
--   calculation of p-value.
kolmogorovSmirnovTest :: Distribution d
                      => d      -- ^ Distribution
                      -> Sample -- ^ Data sample
                      -> Test StatisticKS
kolmogorovSmirnovTest d = kolmogorovSmirnovTestCdf (cumulative d)

-- | Variant of 'kolmogorovSmirnovTest' which uses CFD in form of
--   function.
kolmogorovSmirnovTestCdf :: (Double -> Double) -- ^ CDF of distribution
                         -> Sample             -- ^ Data sample
                         -> Test StatisticKS
kolmogorovSmirnovTestCdf cdf sample
  = Test (confLevel prob) (StatisticKS d)
  where
    d    = kolmogorovSmirnovCdfD cdf sample
    prob = kolmogorovSmirnovProbability (U.length sample) d

-- | Two sample Kolmogorov-Smirnov test. It tests whether two data
--   samples could be described by the same distribution without
--   making any assumptions about it.
--
--   This test uses approxmate formula for computing p-value.
kolmogorovSmirnovTest2 :: Sample -- ^ Sample 1
                       -> Sample -- ^ Sample 2
                       -> Test StatisticKS
kolmogorovSmirnovTest2 xs1 xs2
  = Test (confLevel $ prob $ d * (en + 0.12 + 0.11/en)) (StatisticKS d)
  where
    d    = kolmogorovSmirnov2D xs1 xs2
    -- Effective number of data points
    n1   = fromIntegral (U.length xs1)
    n2   = fromIntegral (U.length xs2)
    en   = sqrt $ n1 * n2 / (n1 + n2)
    --
    prob z
      | z <  0    = error "kolmogorovSmirnov2D: internal error"
      | z == 0    = 1
      | z <  1.18 = let y = exp( -1.23370055013616983 / (z*z) )
                    in  2.25675833419102515 * sqrt( -log(y) ) * (y + y**9 + y**25 + y**49)
      | otherwise = let x = exp(-2 * z * z)
                    in  1 - 2*(x - x**4 + x**9)
-- FIXME: Find source for approximation for D

-- | Test statistics for Kolmogorov-Smirnov test.
data StatisticKS = StatisticKS { statisticKS :: Double }
                   deriving (Show,Eq,Typeable,Data,Generic)
instance Binary   StatisticKS
instance FromJSON StatisticKS
instance ToJSON   StatisticKS
instance NFData   StatisticKS



----------------------------------------------------------------
-- Kolmogorov's statistic
----------------------------------------------------------------

-- | Calculate Kolmogorov's statistic /D/ for given cumulative
--   distribution function (CDF) and data sample. If sample is empty
--   returns 0.
kolmogorovSmirnovCdfD :: (Double -> Double) -- ^ CDF function
                      -> Sample             -- ^ Sample
                      -> Double
kolmogorovSmirnovCdfD cdf sample
  | U.null sample = 0
  | otherwise     = U.maximum
                  $ U.zipWith3 (\p a b -> abs (p-a) `max` abs (p-b))
                    ps steps (U.tail steps)
  where
    xs = sort sample
    n  = U.length xs
    --
    ps    = U.map cdf xs
    steps = U.map ((/ fromIntegral n) . fromIntegral)
          $ U.generate (n+1) id


-- | Calculate Kolmogorov's statistic /D/ for given cumulative
--   distribution function (CDF) and data sample. If sample is empty
--   returns 0.
kolmogorovSmirnovD :: (Distribution d)
                   => d         -- ^ Distribution
                   -> Sample    -- ^ Sample
                   -> Double
kolmogorovSmirnovD d = kolmogorovSmirnovCdfD (cumulative d)

-- | Calculate Kolmogorov's statistic /D/ for two data samples. If
--   either of samples is empty returns 0.
kolmogorovSmirnov2D :: Sample   -- ^ First sample
                    -> Sample   -- ^ Second sample
                    -> Double
kolmogorovSmirnov2D sample1 sample2
  | U.null sample1 || U.null sample2 = 0
  | otherwise                        = worker 0 0 0
  where
    xs1 = sort sample1
    xs2 = sort sample2
    n1  = U.length xs1
    n2  = U.length xs2
    en1 = fromIntegral n1
    en2 = fromIntegral n2
    -- Find new index
    skip x i xs = go (i+1)
      where go n | n >= U.length xs = n
                 | xs U.! n == x    = go (n+1)
                 | otherwise        = n
    -- Main loop
    worker d i1 i2
      | i1 >= n1 || i2 >= n2 = d
      | otherwise            = worker d' i1' i2'
      where
        d1  = xs1 U.! i1
        d2  = xs2 U.! i2
        i1' | d1 <= d2  = skip d1 i1 xs1
            | otherwise = i1
        i2' | d2 <= d1  = skip d2 i2 xs2
            | otherwise = i2
        d'  = max d (abs $ fromIntegral i1' / en1 - fromIntegral i2' / en2)



-- | Calculate cumulative probability function for Kolmogorov's
--   distribution with /n/ parameters or probability of getting value
--   smaller than /d/ with n-elements sample.
--
--   It uses algorithm by Marsgalia et. al. and provide at least
--   7-digit accuracy.
kolmogorovSmirnovProbability :: Int    -- ^ Size of the sample
                             -> Double -- ^ D value
                             -> Double
kolmogorovSmirnovProbability n d
  -- Avoid potencially lengthy calculations for large N and D > 0.999
  | s > 7.24 || (s > 3.76 && n > 99) = 1 - 2 * exp( -(2.000071 + 0.331 / sqrt n' + 1.409 / n') * s)
  -- Exact computation
  | otherwise = fini $ matrix `power` n
  where
    s  = n' * d * d
    n' = fromIntegral n

    size = 2*k - 1
    k    = floor (n' * d) + 1
    h    = fromIntegral k - n' * d
    -- Calculate initial matrix
    matrix =
      let m = U.create $ do
            mat <- M.new (size*size)
            -- Fill matrix with 0 and 1s
            for 0 size $ \row ->
              for 0 size $ \col -> do
                let val | row + 1 >= col = 1
                        | otherwise      = 0 :: Double
                M.write mat (row * size + col) val
            -- Correct left column/bottom row
            for 0 size $ \i -> do
              let delta = h ^^ (i + 1)
              unsafeModify mat (i    * size)         (subtract delta)
              unsafeModify mat (size * size - 1 - i) (subtract delta)
            -- Correct corner element if needed
            when (2*h > 1) $ do
              unsafeModify mat ((size - 1) * size) (+ ((2*h - 1) ^ size))
            -- Divide diagonals by factorial
            let divide g num
                  | num == size = return ()
                  | otherwise   = do for num size $ \i ->
                                       unsafeModify mat (i * (size + 1) - num) (/ g)
                                     divide (g * fromIntegral (num+2)) (num+1)
            divide 2 1
            return mat
      in fromVector size size m
    -- Last calculation
    fini m = loop 1 (center m) (exponent m)
      where
        loop i ss eQ
          | i  > n       = ss * 10 ^^ eQ
          | ss' < 1e-140 = loop (i+1) (ss' * 1e140) (eQ - 140)
          | otherwise    = loop (i+1)  ss'           eQ
          where ss' = ss * fromIntegral i / fromIntegral n

----------------------------------------------------------------

-- $references
--
-- * G. Marsaglia, W. W. Tsang, J. Wang (2003) Evaluating Kolmogorov's
--   distribution, Journal of Statistical Software, American
--   Statistical Association, vol. 8(i18).
