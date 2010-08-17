-- |
-- Module    : Statistics.Test.NonParametric
-- Copyright : (c) 2010 Neil Brown
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for performing non-parametric tests (i.e. tests without an assumption
-- of underlying distribution).
module Statistics.Test.NonParametric
  (-- Wilcoxon rank sum test
  wilcoxonRankSums,
  -- * Mann-Whitney U test
  mannWhitneyU,
   -- * Wilcoxon signed-rank matched-pair test
   -- This test is the non-parametric equivalent to the paired t-test
  wilcoxonMatchedPairSignedRank, wilcoxonSignificant, wilcoxonSignificance, wilcoxonCriticalValue) where

import Control.Arrow ((***))
import Data.Function (on)
import Data.List (findIndex, groupBy, partition, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U (length, toList, zipWith)

import Statistics.Types (Sample)

-- | The Wilcoxon Rank Sums Test.
--
-- This test calculates the sum of ranks for the given two samples.  The samples
-- are ordered, and assigned ranks (ties are given their average rank), then these
-- ranks are summed for each sample.
--
-- The return value is (W_1, W_2) where W_1 is the sum of ranks of the first sample
-- and W_2 is the sum of ranks of the second sample.  This test is trivially transformed
-- into the Mann-Whitney U test.  You will probably want to use 'mannWhitneyU'
-- and the related functions for testing significance, but this function is exposed
-- for completeness.
wilcoxonRankSums :: Sample -> Sample -> (Double, Double)
wilcoxonRankSums xs1 xs2
  = ((sum . map fst) *** (sum . map fst)) . -- sum the ranks per group
    partition snd . -- split them back into left and right
    concatMap mergeRanks . -- merge the ranks of duplicates
    groupBy ((==) `on` (snd . snd)) . -- group duplicate values
    zip [1..] . -- give them ranks (duplicates receive different ranks here)
    sortBy (comparing snd) $ -- sort by their values
    zip (repeat True) (U.toList xs1) ++ zip (repeat False) (U.toList xs2)
      -- Tag each sample with an identifier before we merge them
  where
    mergeRanks :: [(AbsoluteRank, (Bool, Double))] -> [(AbsoluteRank, Bool)]
    mergeRanks xs = zip (repeat rank) (map (fst . snd) xs)
      where
        -- Ranks are merged by assigning them all the average of their ranks:
        rank = sum (map fst xs) / fromIntegral (length xs)

-- | The Mann-Whitney U Test.
--
-- This is sometimes known as the Mann-Whitney-Wilcoxon U test, and
-- confusingly many sources state that the Mann-Whitney U test is the same as
-- the Wilcoxon's rank sum test (which is provided as 'wilcoxonRankSums').
-- The Mann-Whitney U is a simple transform of Wilcoxon's rank sum test.
--
-- Again confusingly, different sources state reversed definitions for U_1 and U_2,
-- so it is worth being explicit about what this function returns.  Given two samples,
-- the first, xs_1, of size n_1 and the second, xs_2, of size n_2, this function
-- returns (U_1, U_2) where U_1 = W_1 - (n_1*(n_1+1))/2 and U_2 = W_2 - (n_2*(n_2+1))/2,
-- where (W_1, W_2) is the return value of @wilcoxonRankSums xs1 xs2@.
--
-- Some sources instead state that U_1 and U_2 should be the other way round, often
-- expressing this using U_1' = n_1*n_2 - U_1 (since U_1 + U_2 = n_1*n*2).
--
-- All of which you probably don't care about if you just feed this into 'mannWhitneyUSignificant'.
mannWhitneyU :: Sample -> Sample -> (Double, Double)
mannWhitneyU xs1 xs2
  = (fst summedRanks - (n1*(n1 + 1))/2
    ,snd summedRanks - (n2*(n2 + 1))/2)
  where
    n1 = fromIntegral $ U.length xs1
    n2 = fromIntegral $ U.length xs2
    
    summedRanks = wilcoxonRankSums xs1 xs2
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
wilcoxonMatchedPairSignedRank a b
  -- Best to read this function bottom to top:
  = (sum *** sum) . -- Sum the positive and negative ranks separately.
    partition (> 0) . -- Split the ranks into positive and negative.  None of the
                      -- ranks can be zero.
    concatMap mergeRanks . -- Then merge the ranks for any duplicates by taking
                           -- the average of the ranks, and also make the rank
                           -- into a signed rank
    groupBy ((==) `on` abs . snd) . -- Now group any duplicates together
                                    -- Note: duplicate means same absolute difference
    zip [1..] . -- Add a rank (note: at this stage, duplicates will get different ranks)
    dropWhile (== 0) . -- Remove any differences that are zero (i.e. ties in the
                       -- original data).  We know they must be at the head of
                       -- the list because we just sorted it, so dropWhile not filter
    sortBy (comparing abs) . -- Sort the differences by absolute difference
    U.toList $ -- Convert to a list (could be done later in the pipeline?)
    U.zipWith (-) a b -- Work out differences
  where
    mergeRanks :: [(AbsoluteRank, Double)] -> [SignedRank]
    mergeRanks xs = map ((* rank) . signum . snd) xs
      -- Note that signum above will always be 1 or -1; any zero differences will
      -- have been removed before this function is called.
      where
        -- Ranks are merged by assigning them all the average of their ranks:
        rank = sum (map fst xs) / fromIntegral (length xs)

type AbsoluteRank = Double
type SignedRank = Double

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
coefficients :: Int -> [Int]
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
summedCoefficients = map fromIntegral . scanl1 (+) . coefficients

-- | Tests whether a given result from a Wilcoxon signed-rank matched-pairs test
-- is significant at the given level.
--
-- This function can perform a one-tailed or two-tailed test.  If the first
-- parameter to this function is False, the test is performed two-tailed to
-- check if the two samples differ significantly.  If the first parameter is
-- True, the check is performed one-tailed to decide whether the first sample
-- (i.e. the first sample you passed to 'wilcoxonMatchedPairSignedRank') is
-- greater than the second sample (i.e. the second sample you passed to
-- 'wilcoxonMatchedPairSignedRank').  If you wish to perform a one-tailed test
-- in the opposite direction, you can either pass the parameters in a different
-- order to 'wilcoxonMatchedPairSignedRank', or simply swap the values in the resulting
-- pair before passing them to this function.
wilcoxonSignificant :: Bool -- ^ Perform one-tailed test (see description above).
                    -> Int  -- ^ The sample size from which the (T+,T-) values were derived.
                    -> Double -- ^ The p-value at which to test (e.g. 0.05)
                    -> (Double, Double) -- ^ The (T+, T-) values from 'wilcoxonMatchedPairSignedRank'.
                    -> Maybe Bool -- ^ Just True if the test is significant, Just
                                  -- False if it is not, and Nothing if the sample
                                  -- was too small to make a decision.
wilcoxonSignificant oneTail sampleSize p (tPlus, tMinus)
  -- According to my nearest book (Understanding Research Methods and Statistics
  -- by Gary W. Heiman, p590), to check that the first sample is bigger you must
  -- use the absolute value of T- for a one-tailed check:
  | oneTail = ((abs tMinus <=) . fromIntegral) `fmap` wilcoxonCriticalValue sampleSize p
  -- Otherwise you must use the value of T+ and T- with the smallest absolute value:
  | otherwise = ((t <=) . fromIntegral) `fmap` wilcoxonCriticalValue sampleSize (p/2)
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
wilcoxonCriticalValue :: Int -- ^ The sample size
                      -> Double -- ^ The p-value (e.g. 0.05) for which you want the critical value.
                      -> Maybe Int -- ^ The critical value (of T), or Nothing if
                                   -- the sample is too small to make a decision.
wilcoxonCriticalValue sampleSize p
  = case critical of
      Just n | n < 0 -> Nothing
             | otherwise -> Just n
      Nothing -> Just maxBound -- shouldn't happen: beyond end of list
  where
    m = (2 ** fromIntegral sampleSize) * p
    critical = subtract 1 `fmap` findIndex (> m) (summedCoefficients sampleSize)

-- | Works out the significance level (p-value) of a T value, given a sample
-- size and a T value from the Wilcoxon signed-rank matched-pairs test.
--
-- See the notes on 'wilcoxonCriticalValue' for how this is calculated.
wilcoxonSignificance :: Int -- ^ The sample size
                     -> Double -- ^ The value of T for which you want the significance.
                     -> Double -- ^^ The significance (p-value).
wilcoxonSignificance sampleSize rank
  = (summedCoefficients sampleSize !! floor rank) / 2 ** fromIntegral sampleSize

