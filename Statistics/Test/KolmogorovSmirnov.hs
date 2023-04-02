{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Test.KolmogorovSmirnov
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Kolmogov-Smirnov tests are non-parametric tests for assessing
-- whether given sample could be described by distribution or whether
-- two samples have the same distribution. It's only applicable to
-- continuous distributions.
module Statistics.Test.KolmogorovSmirnov (
    -- * Kolmogorov-Smirnov test
    kolmogorovSmirnovTest
  , kolmogorovSmirnovTestCdf
  , kolmogorovSmirnovTest2
    -- * Evaluate statistics
  , kolmogorovSmirnovCdfD
  , kolmogorovSmirnovD
  , kolmogorovSmirnov2D
    -- * Probabilities
  , kolmogorovSmirnovProbability
    -- * References
    -- $references
  , module Statistics.Test.Types
  ) where

import Control.Monad (when)
import Prelude hiding (exponent, sum)
import Statistics.Distribution (Distribution(..))
import Statistics.Function (gsort, unsafeModify)
import Statistics.Matrix (center, for, fromVector)
import qualified Statistics.Matrix as Mat
import Statistics.Test.Types
import Statistics.Types (mkPValue)
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Generic  as G
import           Data.Vector.Generic    ((!))
import qualified Data.Vector.Unboxed.Mutable as M


----------------------------------------------------------------
-- Test
----------------------------------------------------------------

-- | Check that sample could be described by distribution. Returns
--   @Nothing@ is sample is empty
--
--   This test uses Marsaglia-Tsang-Wang exact algorithm for
--   calculation of p-value.
kolmogorovSmirnovTest :: (Distribution d, G.Vector v Double)
                      => d        -- ^ Distribution
                      -> v Double -- ^ Data sample
                      -> Maybe (Test ())
{-# INLINE kolmogorovSmirnovTest #-}
kolmogorovSmirnovTest d
  = kolmogorovSmirnovTestCdf (cumulative d)


-- | Variant of 'kolmogorovSmirnovTest' which uses CDF in form of
--   function.
kolmogorovSmirnovTestCdf :: (G.Vector v Double)
                         => (Double -> Double) -- ^ CDF of distribution
                         -> v Double           -- ^ Data sample
                         -> Maybe (Test ())
{-# INLINE kolmogorovSmirnovTestCdf #-}
kolmogorovSmirnovTestCdf cdf sample
  | G.null sample = Nothing
  | otherwise     = Just Test
      { testSignificance = mkPValue $ 1 - prob
      , testStatistics   = d
      , testDistribution = ()
      }
  where
    d    = kolmogorovSmirnovCdfD cdf sample
    prob = kolmogorovSmirnovProbability (G.length sample) d


-- | Two sample Kolmogorov-Smirnov test. It tests whether two data
--   samples could be described by the same distribution without
--   making any assumptions about it. If either of samples is empty
--   returns Nothing.
--
--   This test uses approximate formula for computing p-value.
kolmogorovSmirnovTest2 :: (G.Vector v Double)
                       => v Double -- ^ Sample 1
                       -> v Double -- ^ Sample 2
                       -> Maybe (Test ())
kolmogorovSmirnovTest2 xs1 xs2
  | G.null xs1 || G.null xs2 = Nothing
  | otherwise                = Just Test
      { testSignificance = mkPValue $ 1 - prob d
      , testStatistics   = d
      , testDistribution = ()
      }
  where
    d    = kolmogorovSmirnov2D xs1 xs2
         * (en + 0.12 + 0.11/en)
    -- Effective number of data points
    n1   = fromIntegral (G.length xs1)
    n2   = fromIntegral (G.length xs2)
    en   = sqrt $ n1 * n2 / (n1 + n2)
    --
    prob z
      | z <  0    = error "kolmogorovSmirnov2D: internal error"
      | z == 0    = 0
      | z <  1.18 = let y = exp( -1.23370055013616983 / (z*z) )
                    in  2.25675833419102515 * sqrt( -log y ) * (y + y**9 + y**25 + y**49)
      | otherwise = let x = exp(-2 * z * z)
                    in  1 - 2*(x - x**4 + x**9)
{-# INLINABLE  kolmogorovSmirnovTest2 #-}
{-# SPECIALIZE kolmogorovSmirnovTest2 :: U.Vector Double -> U.Vector Double -> Maybe (Test ()) #-}
{-# SPECIALIZE kolmogorovSmirnovTest2 :: V.Vector Double -> V.Vector Double -> Maybe (Test ()) #-}
{-# SPECIALIZE kolmogorovSmirnovTest2 :: S.Vector Double -> S.Vector Double -> Maybe (Test ()) #-}
-- FIXME: Find source for approximation for D



----------------------------------------------------------------
-- Kolmogorov's statistic
----------------------------------------------------------------

-- | Calculate Kolmogorov's statistic /D/ for given cumulative
--   distribution function (CDF) and data sample. If sample is empty
--   returns 0.
kolmogorovSmirnovCdfD :: G.Vector v Double
                      => (Double -> Double) -- ^ CDF function
                      -> v Double           -- ^ Sample
                      -> Double
kolmogorovSmirnovCdfD cdf sample
  | G.null sample = 0
  | otherwise     = G.maximum
                  $ G.zipWith3 (\p a b -> abs (p-a) `max` abs (p-b))
                    ps steps (G.tail steps)
  where
    xs = gsort sample
    n  = G.length xs
    --
    ps    = G.map cdf xs
    steps = G.map (/ fromIntegral n)
          $ G.generate (n+1) fromIntegral
{-# INLINABLE  kolmogorovSmirnovCdfD #-}
{-# SPECIALIZE kolmogorovSmirnovCdfD :: (Double -> Double) -> U.Vector Double -> Double #-}
{-# SPECIALIZE kolmogorovSmirnovCdfD :: (Double -> Double) -> V.Vector Double -> Double #-}
{-# SPECIALIZE kolmogorovSmirnovCdfD :: (Double -> Double) -> S.Vector Double -> Double #-}


-- | Calculate Kolmogorov's statistic /D/ for given cumulative
--   distribution function (CDF) and data sample. If sample is empty
--   returns 0.
kolmogorovSmirnovD :: (Distribution d, G.Vector v Double)
                   => d         -- ^ Distribution
                   -> v Double  -- ^ Sample
                   -> Double
kolmogorovSmirnovD d = kolmogorovSmirnovCdfD (cumulative d)
{-# INLINE kolmogorovSmirnovD #-}


-- | Calculate Kolmogorov's statistic /D/ for two data samples. If
--   either of samples is empty returns 0.
kolmogorovSmirnov2D :: (G.Vector v Double)
                    => v Double   -- ^ First sample
                    -> v Double   -- ^ Second sample
                    -> Double
kolmogorovSmirnov2D sample1 sample2
  | G.null sample1 || G.null sample2 = 0
  | otherwise                        = worker 0 0 0
  where
    xs1 = gsort sample1
    xs2 = gsort sample2
    n1  = G.length xs1
    n2  = G.length xs2
    en1 = fromIntegral n1
    en2 = fromIntegral n2
    -- Find new index
    skip x i xs = go (i+1)
      where go n | n >= G.length xs = n
                 | xs ! n == x      = go (n+1)
                 | otherwise        = n
    -- Main loop
    worker d i1 i2
      | i1 >= n1 || i2 >= n2 = d
      | otherwise            = worker d' i1' i2'
      where
        d1  = xs1 ! i1
        d2  = xs2 ! i2
        i1' | d1 <= d2  = skip d1 i1 xs1
            | otherwise = i1
        i2' | d2 <= d1  = skip d2 i2 xs2
            | otherwise = i2
        d'  = max d (abs $ fromIntegral i1' / en1 - fromIntegral i2' / en2)
{-# INLINABLE  kolmogorovSmirnov2D #-}
{-# SPECIALIZE kolmogorovSmirnov2D :: U.Vector Double -> U.Vector Double -> Double #-}
{-# SPECIALIZE kolmogorovSmirnov2D :: V.Vector Double -> V.Vector Double -> Double #-}
{-# SPECIALIZE kolmogorovSmirnov2D :: S.Vector Double -> S.Vector Double -> Double #-}



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
  -- Avoid potentially lengthy calculations for large N and D > 0.999
  | s > 7.24 || (s > 3.76 && n > 99) = 1 - 2 * exp( -(2.000071 + 0.331 / sqrt n' + 1.409 / n') * s)
  -- Exact computation
  | otherwise = fini $ KSMatrix 0 matrix `power` n
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
    fini (KSMatrix e m) = loop 1 (center m) e
      where
        loop i ss eQ
          | i  > n       = ss * 10 ^^ eQ
          | ss' < 1e-140 = loop (i+1) (ss' * 1e140) (eQ - 140)
          | otherwise    = loop (i+1)  ss'           eQ
          where ss' = ss * fromIntegral i / fromIntegral n

data KSMatrix = KSMatrix Int Mat.Matrix


multiply :: KSMatrix -> KSMatrix -> KSMatrix
multiply (KSMatrix e1 m1) (KSMatrix e2 m2) = KSMatrix (e1+e2) (Mat.multiply m1 m2)

power :: KSMatrix -> Int -> KSMatrix
power mat 1 = mat
power mat n = avoidOverflow res
  where
    mat2 = power mat (n `quot` 2)
    pow  = multiply mat2 mat2
    res | odd n     = multiply pow mat
        | otherwise = pow

avoidOverflow :: KSMatrix -> KSMatrix
avoidOverflow ksm@(KSMatrix e m)
  | center m > 1e140 = KSMatrix (e + 140) (Mat.map (* 1e-140) m)
  | otherwise        = ksm


----------------------------------------------------------------

-- $references
--
-- * G. Marsaglia, W. W. Tsang, J. Wang (2003) Evaluating Kolmogorov's
--   distribution, Journal of Statistical Software, American
--   Statistical Association, vol. 8(i18).
