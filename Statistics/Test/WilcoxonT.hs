-- |
-- Module    : Statistics.Test.WilcoxonT
-- Copyright : (c) 2010 Neil Brown
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Wilcoxon matched-pairs signed-rank test is non-parametric test
-- which could be used to whether two related samples have different
-- means.
--
-- WARNING: current implementation contain critical bugs
-- <https://github.com/bos/statistics/issues/18>
module Statistics.Test.WilcoxonT (
    -- * Wilcoxon signed-rank matched-pair test
    wilcoxonMatchedPairTest
  , wilcoxonMatchedPairSignedRank
  , wilcoxonMatchedPairSignificant
  , wilcoxonMatchedPairSignificance
  , wilcoxonMatchedPairCriticalValue
    -- * Data types
  , TestType(..)
  , TestResult(..)
  ) where

import Control.Applicative ((<$>))
import Data.Function       (on)
import Data.List           (findIndex)
import Data.Ord            (comparing)
import qualified Data.Vector.Unboxed as U

import Statistics.Types               (Sample)
import Statistics.Function            (sortBy)
import Statistics.Test.Types
import Statistics.Test.Internal


-- | The Wilcoxon matched-pairs signed-rank test.
--
-- The value returned is the pair (T+, T-).  T+ is the sum of positive ranks (the
-- ranks of the differences where the first parameter is higher) whereas T- is
-- the sum of negative ranks (the ranks of the differences where the second parameter is higher).
-- These values mean little by themselves, and should be combined with the 'wilcoxonSignificant'
-- function in this module to get a meaningful result.
--
-- The samples are zipped together: if one is longer than the other, both are truncated
-- to the the length of the shorter sample.
--
-- Note that: wilcoxonMatchedPairSignedRank == (\(x, y) -> (y, x)) . flip wilcoxonMatchedPairSignedRank
wilcoxonMatchedPairSignedRank :: Sample -> Sample -> (Double, Double)
wilcoxonMatchedPairSignedRank a b = (          U.sum ranks1
                                    , negate $ U.sum ranks2
                                    )
  where
    (ranks1, ranks2) = splitByTags
                     $ U.zip tags (rank ((==) `on` abs) diffs)
    (tags,diffs) = U.unzip
                 $ U.map (\x -> (x>0 , x))   -- Attack tags to distribution elements
                 $ U.filter  (/= 0.0)        -- Remove equal elements
                 $ sortBy (comparing abs)    -- Sort the differences by absolute difference
                 $ U.zipWith (-) a b         -- Work out differences


-- | The coefficients for x^0, x^1, x^2, etc, in the expression
-- \prod_{r=1}^s (1 + x^r).  See the Mitic paper for details.
--
-- We can define:
-- f(1) = 1 + x
-- f(r) = (1 + x^r)*f(r-1)
--      = f(r-1) + x^r * f(r-1)
-- The effect of multiplying the equation by x^r is to shift
-- all the coefficients by r down the list.
--
-- This list will be processed lazily from the head.
coefficients :: Int -> [Integer]
coefficients 1 = [1, 1] -- 1 + x
coefficients r = let coeffs = coefficients (r-1)
                     (firstR, rest) = splitAt r coeffs
  in firstR ++ add rest coeffs
  where
    add (x:xs) (y:ys) = x + y : add xs ys
    add xs [] = xs
    add [] ys = ys

-- This list will be processed lazily from the head.
summedCoefficients :: Int -> [Double]
summedCoefficients n
  | n < 1     = error "Statistics.Test.WilcoxonT.summedCoefficients: nonpositive sample size"
  | n > 1023  = error "Statistics.Test.WilcoxonT.summedCoefficients: sample is too large (see bug #18)"
  | otherwise = map fromIntegral $ scanl1 (+) $ coefficients n

-- | Tests whether a given result from a Wilcoxon signed-rank matched-pairs test
-- is significant at the given level.
--
-- This function can perform a one-tailed or two-tailed test.  If the first
-- parameter to this function is 'TwoTailed', the test is performed two-tailed to
-- check if the two samples differ significantly.  If the first parameter is
-- 'OneTailed', the check is performed one-tailed to decide whether the first sample
-- (i.e. the first sample you passed to 'wilcoxonMatchedPairSignedRank') is
-- greater than the second sample (i.e. the second sample you passed to
-- 'wilcoxonMatchedPairSignedRank').  If you wish to perform a one-tailed test
-- in the opposite direction, you can either pass the parameters in a different
-- order to 'wilcoxonMatchedPairSignedRank', or simply swap the values in the resulting
-- pair before passing them to this function.
wilcoxonMatchedPairSignificant ::
     TestType            -- ^ Perform one- or two-tailed test (see description below).
  -> Int                 -- ^ The sample size from which the (T+,T-) values were derived.
  -> Double              -- ^ The p-value at which to test (e.g. 0.05)
  -> (Double, Double)    -- ^ The (T+, T-) values from 'wilcoxonMatchedPairSignedRank'.
  -> Maybe TestResult    -- ^ Return 'Nothing' if the sample was too
                         --   small to make a decision.
wilcoxonMatchedPairSignificant test sampleSize p (tPlus, tMinus) =
  case test of
    -- According to my nearest book (Understanding Research Methods and Statistics
    -- by Gary W. Heiman, p590), to check that the first sample is bigger you must
    -- use the absolute value of T- for a one-tailed check:
    OneTailed -> (significant . (abs tMinus <=) . fromIntegral) <$> wilcoxonMatchedPairCriticalValue sampleSize p
    -- Otherwise you must use the value of T+ and T- with the smallest absolute value:
    TwoTailed -> (significant . (t <=) . fromIntegral) <$> wilcoxonMatchedPairCriticalValue sampleSize (p/2)
  where
    t = min (abs tPlus) (abs tMinus)

-- | Obtains the critical value of T to compare against, given a sample size
-- and a p-value (significance level).  Your T value must be less than or
-- equal to the return of this function in order for the test to work out
-- significant.  If there is a Nothing return, the sample size is too small to
-- make a decision.
--
-- 'wilcoxonSignificant' tests the return value of 'wilcoxonMatchedPairSignedRank'
-- for you, so you should use 'wilcoxonSignificant' for determining test results.
--  However, this function is useful, for example, for generating lookup tables
-- for Wilcoxon signed rank critical values.
--
-- The return values of this function are generated using the method detailed in
-- the paper \"Critical Values for the Wilcoxon Signed Rank Statistic\", Peter
-- Mitic, The Mathematica Journal, volume 6, issue 3, 1996, which can be found
-- here: <http://www.mathematica-journal.com/issue/v6i3/article/mitic/contents/63mitic.pdf>.
-- According to that paper, the results may differ from other published lookup tables, but
-- (Mitic claims) the values obtained by this function will be the correct ones.
wilcoxonMatchedPairCriticalValue ::
     Int                -- ^ The sample size
  -> Double             -- ^ The p-value (e.g. 0.05) for which you want the critical value.
  -> Maybe Int          -- ^ The critical value (of T), or Nothing if
                        --   the sample is too small to make a decision.
wilcoxonMatchedPairCriticalValue sampleSize p
  = case critical of
      Just n | n < 0 -> Nothing
             | otherwise -> Just n
      Nothing -> Just maxBound -- shouldn't happen: beyond end of list
  where
    m = (2 ** fromIntegral sampleSize) * p
    critical = subtract 1 <$> findIndex (> m) (summedCoefficients sampleSize)

-- | Works out the significance level (p-value) of a T value, given a sample
-- size and a T value from the Wilcoxon signed-rank matched-pairs test.
--
-- See the notes on 'wilcoxonCriticalValue' for how this is calculated.
wilcoxonMatchedPairSignificance :: Int    -- ^ The sample size
                                -> Double -- ^ The value of T for which you want the significance.
                                -> Double -- ^ The significance (p-value).
wilcoxonMatchedPairSignificance sampleSize rnk
  = (summedCoefficients sampleSize !! floor rnk) / 2 ** fromIntegral sampleSize

-- | The Wilcoxon matched-pairs signed-rank test. The samples are
-- zipped together: if one is longer than the other, both are
-- truncated to the the length of the shorter sample.
--
-- For one-tailed test it tests whether first sample is significantly
-- greater than the second. For two-tailed it checks whether they
-- significantly differ
--
-- Check 'wilcoxonMatchedPairSignedRank' and
-- 'wilcoxonMatchedPairSignificant' for additional information.
wilcoxonMatchedPairTest :: TestType   -- ^ Perform one-tailed test.
                        -> Double     -- ^ The p-value at which to test (e.g. 0.05)
                        -> Sample     -- ^ First sample
                        -> Sample     -- ^ Second sample
                        -> Maybe TestResult
                        -- ^ Return 'Nothing' if the sample was too
                        --   small to make a decision.
wilcoxonMatchedPairTest test p smp1 smp2 =
    wilcoxonMatchedPairSignificant test (min n1 n2) p
  $ wilcoxonMatchedPairSignedRank smp1 smp2
  where
    n1 = U.length smp1
    n2 = U.length smp2
