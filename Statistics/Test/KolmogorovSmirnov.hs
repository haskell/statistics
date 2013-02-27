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
    -- * Kolmogorov-Smirnov test
    kolmogorovSmirnovTest
  , kolmogorovSmirnovTestCdf
  , kolmogorovSmirnovTest2
    -- * Evaluate statistics
  , kolmogorovSmirnovCdfD
  , kolmogorovSmirnovD
  , kolmogorovSmirnov2D
    -- * Probablities
  , kolmogorovSmirnovProbability
    -- * Data types
  , TestType(..)
  , TestResult(..)
    -- * References
    -- $references
  ) where

import Control.Monad
import Control.Monad.ST  (ST)

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import Statistics.Distribution        (Distribution(..))
import Statistics.Types               (Sample)
import Statistics.Function            (sort)
import Statistics.Test.Types

import Text.Printf



----------------------------------------------------------------
-- Test
----------------------------------------------------------------

-- | Check that sample could be described by
--   distribution. 'Significant' means distribution is not compatible
--   with data for given p-value.
--
--   This test uses Marsaglia-Tsang-Wang exact alogorithm for
--   calculation of p-value.
kolmogorovSmirnovTest :: Distribution d
                      => d      -- ^ Distribution
                      -> Double -- ^ p-value
                      -> Sample -- ^ Data sample
                      -> TestResult
kolmogorovSmirnovTest d = kolmogorovSmirnovTestCdf (cumulative d)
{-# INLINE kolmogorovSmirnovTest #-}

-- | Variant of 'kolmogorovSmirnovTest' which uses CFD in form of
--   function.
kolmogorovSmirnovTestCdf :: (Double -> Double) -- ^ CDF of distribution
                         -> Double             -- ^ p-value
                         -> Sample             -- ^ Data sample
                         -> TestResult
kolmogorovSmirnovTestCdf cdf p sample
  | p > 0 && p < 1 = significant $ 1 - prob < p
  | otherwise      = error "Statistics.Test.KolmogorovSmirnov.kolmogorovSmirnovTestCdf:bad p-value"
  where
    d    = kolmogorovSmirnovCdfD cdf sample
    prob = kolmogorovSmirnovProbability (U.length sample) d

-- | Two sample Kolmogorov-Smirnov test. It tests whether two data
--   samples could be described by the same distribution without
--   making any assumptions about it.
--
--   This test uses approxmate formula for computing p-value.
kolmogorovSmirnovTest2 :: Double -- ^ p-value
                       -> Sample -- ^ Sample 1
                       -> Sample -- ^ Sample 2
                       -> TestResult
kolmogorovSmirnovTest2 p xs1 xs2
  | p > 0 && p < 1 = significant $ 1 - prob( d*(en + 0.12 + 0.11/en) ) < p
  | otherwise      = error "Statistics.Test.KolmogorovSmirnov.kolmogorovSmirnovTest2:bad p-value"
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
  | U.null xs = 0
  | otherwise = U.maximum
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
{-# INLINE kolmogorovSmirnovD #-}

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
  | otherwise = fini $ matrixPower matrix n
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
              modify mat (i    * size)         (subtract delta)
              modify mat (size * size - 1 - i) (subtract delta)
            -- Correct corner element if needed
            when (2*h > 1) $ do
              modify mat ((size - 1) * size) (+ ((2*h - 1) ^ size))
            -- Divide diagonals by factorial
            let divide g num
                  | num == size = return ()
                  | otherwise   = do for num size $ \i ->
                                       modify mat (i * (size + 1) - num) (/ g)
                                     divide (g * fromIntegral (num+2)) (num+1)
            divide 2 1
            return mat
      in Matrix size m 0
    -- Last calculation
    fini m@(Matrix _ _ e) = loop 1 (matrixCenter m) e
      where
        loop i ss eQ
          | i  > n       = ss * 10 ^^ eQ
          | ss' < 1e-140 = loop (i+1) (ss' * 1e140) (eQ - 140)
          | otherwise    = loop (i+1)  ss'           eQ
          where ss' = ss * fromIntegral i / fromIntegral n


----------------------------------------------------------------

-- Maxtrix operations.
--
-- There isn't the matrix package for haskell yet so nessesary minimum
-- is implemented here.

-- Square matrix stored in row-major order
data Matrix = Matrix
              {-# UNPACK #-} !Int -- Size of matrix
              !(U.Vector Double)  -- Matrix data
              {-# UNPACK #-} !Int -- In order to avoid overflows
                                  -- during matrix multiplication large
                                  -- exponent is stored seprately

-- Show instance useful mostly for debugging
instance Show Matrix where
  show (Matrix n vs _) = unlines $ map (unwords . map (printf "%.4f")) $ split $ U.toList vs
    where
      split [] = []
      split xs = row : split rest where (row, rest) = splitAt n xs


-- Avoid overflow in the matrix
avoidOverflow :: Matrix -> Matrix
avoidOverflow m@(Matrix n xs e)
  | matrixCenter m > 1e140 = Matrix n (U.map (* 1e-140) xs) (e + 140)
  | otherwise              = m

-- Unsafe matrix-matrix multiplication. Matrices must be of the same
-- size. This is not checked.
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply (Matrix n xs e1) (Matrix _ ys e2) =
  Matrix n (U.generate (n*n) go) (e1 + e2)
  where
    go i = U.sum $ U.zipWith (*) row col
      where
        nCol = i `rem` n
        row  = U.slice (i - nCol) n xs
        col  = U.backpermute ys $ U.enumFromStepN nCol n n

-- Raise matrix to power N. power must be positive it's not checked
matrixPower :: Matrix -> Int -> Matrix
matrixPower mat 1 = mat
matrixPower mat n = avoidOverflow res
  where
    mat2 = matrixPower mat (n `quot` 2)
    pow  = matrixMultiply mat2 mat2
    res | odd n     = matrixMultiply pow mat
        | otherwise = pow

-- Element in the center of matrix (Not corrected for exponent)
matrixCenter :: Matrix -> Double
matrixCenter (Matrix n xs _) = (U.!) xs (k*n + k) where k = n `quot` 2

-- Simple for loop
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = f i >> loop (i+1)

-- Modify element in the vector
modify :: U.Unbox a => M.MVector s a -> Int -> (a -> a) -> ST s ()
modify arr i f = do x <- M.read arr i
                    M.write arr i (f x)
{-# INLINE modify #-}

----------------------------------------------------------------

-- $references
--
-- * G. Marsaglia, W. W. Tsang, J. Wang (2003) Evaluating Kolmogorov's
--   distribution, Journal of Statistical Software, American
--   Statistical Association, vol. 8(i18).
