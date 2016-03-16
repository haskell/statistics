
-----------------------------------------------------------------------------
-- |
-- Module      :  Statistics.Test.TwoSampleT
-- Copyright   :  (C) 2016 Ethan Pailes,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Ethan Pailes <ethanpailes@gmail.com>
-- Stability   :  provisional
--
-- Note that this statistic is only valid if the standard
-- deviations of the two samples are roughly the same. We recommend that
-- this property of the data is checked with an F test before the
-- two sample test is applied. All functions currently provided by
-- this module are for two tailed tests.
--  
----------------------------------------------------------------------------
module Statistics.Test.TwoSampleT (
    twoSampleStudentT
  , twoSampleStudentTVerdict
  , twoSampleStudentTStatistic
  , TestResult(..)
 ) where

import qualified Data.Vector.Unboxed as V

import Statistics.Test.Types

import Statistics.Distribution hiding (stdDev)
import Statistics.Distribution.StudentT


-- | Tests the alternative hypothesis that the true means of the
--   two provided samples are not equal to one another.
twoSampleStudentT :: V.Vector Double -- ^ the first sample
                  -> V.Vector Double -- ^ the second sample
                  -> Double          -- ^ the p-value
twoSampleStudentT xs ys = (complCumulative t observedTPos) * 2
    where n = V.length xs
          m = V.length ys
          t = studentT $ fromIntegral (n + m - 2)
          observedTPos = abs $ twoSampleStudentTStatistic xs ys

-- One nice thing about the two sample hypothesis is that there is always
-- a natural $H_0$, namely $\mu_x = \mu_y$. There are still three alternative
-- hypothesis:
--    a) $\mu_x > \mu_y$: Reject $H_0$ if
--      \[
--         t \geq t_{\alpha, n + m - 2}
--      \]
--    b) $\mu_x < \mu_y$: Reject $H_0$ if
--      \[
--         t \leq - t_{\alpha, n + m - 2}
--      \]
--    c) $\mu_x \neq \mu_y$: Reject $H_0$ if
--      \[
--         t \geq t_{\frac{\alpha}{2}, n + m - 2}
--      \]
--      \noindent or
--      \]
--         t \leq - t_{\frac{\alpha}{2}, n + m - 2}
--      \]
--
--  Each of these functions can be stated as a boolean function in
--  terms of the p-value for a given sample.

-- | Tests the alternative hypothesis that the true mean of the two
--   provided samples are not equal to one another. Returns a simple
--   verdict of 'Significant' or 'NotSignificant' at the provided significance
--   (alpha value). 'twoSampleStudentT' provides strictly more information,
--   but it is very often useful to specialize a hypothesis test to a
--   given level of significance.
twoSampleStudentTVerdict :: Double -- ^ alpha, the probability of a Type I error
                         -> V.Vector Double -- ^ the first sample
                         -> V.Vector Double -- ^ the second sample
                         -> TestResult      -- ^ the verdict
twoSampleStudentTVerdict alpha xs ys =
  significant $ twoSampleStudentT xs ys < alpha



-- Let $\{x_i\}_{i=0}^n$ and $\{y_i\}_{i=0}^m$ be the two samples.
-- Then the two sample student-t statistic, t, is $T_{n+m-2}$ distributed
-- it is given by the following formula
--
--  \[
--      t =
--      \frac{
--        \overline{x} - \overline{y}
--      }{
--        S_p \sqrt{ \frac{1}{m} + \frac{1}{n} }
--      }
--  \]
--
--  where $S_p^2$ is the pooled variance estimator given by
--
--    \[
--        S_p^2 =
--        \frac{
--            \Sigma_{i=1}^n (X_i - \overline{X})^2
--          + \Sigma_{i=1}^m (Y_i - \overline{Y})^2
--        }{
--          n + m - 2
--        }
--    \]
--
--  Source: Larsen & Marx
--

-- | Computes the observed t value given two samples. This value comes
--   from a t distribution with (n + m - 2) degrees of freedom where
--   n is the size of the first sample and m is the size of the second
--   sample.
twoSampleStudentTStatistic :: V.Vector Double -- ^ the first sample
                           -> V.Vector Double -- ^ the second sample
                           -> Double          -- ^ the observed t value
twoSampleStudentTStatistic xs ys =
      (xBar - yBar) / (stdDev * (sqrt (1/n + 1/m)))
  where n = fromIntegral (V.length xs)
        m = fromIntegral (V.length ys)
        xBar = V.sum xs / n
        yBar = V.sum ys / m
        stdDev = sqrt $ pooledSampleVariance xs ys
-- verified against the R t.test() function

-- | Computes the pooled sample variance of 
pooledSampleVariance :: V.Vector Double
                     -> V.Vector Double
                     -> Double
pooledSampleVariance xs ys = (xSumSquares + ySumSquares) / (n + m - 2)
  where n = fromIntegral (V.length xs)
        m = fromIntegral (V.length ys)
        xBar = V.sum xs / n
        yBar = V.sum ys / m
        xSumSquares = sumWith (\x -> (x - xBar)^2) xs
        ySumSquares = sumWith (\y -> (y - yBar)^2) ys

-- | Think capital Sigma in a math formula
sumWith :: (Num a, V.Unbox a) => (a -> a) -> V.Vector a -> a
sumWith f = V.foldr (\i acc -> acc + f i) 0



-- $references
--
-- * Richard J. Larsen, Morris  L. Marx (2012) An Introduction
--   to Mathematical Statistics and Its Applications - 5th ed,
--   Prentice Hall, ISBN 978-0-321-69394-5
