{-# LANGUAGE FlexibleContexts #-}
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

-- HADDOCK NOTE
--   &#8321; is 1 subscript
--   &#8322; is 2 subscript
module Statistics.Test.NonParametric
  ( -- * Mann-Whitney U test
    mannWhitneyUtest
  , mannWhitneyU
  , mannWhitneyUCriticalValue
  , mannWhitneyUSignificant
    -- ** Wilcoxon rank sum test
  , wilcoxonRankSums
    -- * Wilcoxon signed-rank matched-pair test
  , wilcoxonMatchedPairTest
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
import qualified Data.Vector.Generic as G

import Statistics.Distribution        (quantile)
import Statistics.Distribution.Normal (standard)
import Statistics.Math                (choose)
import Statistics.Types               (Sample)
import Statistics.Function            (sortBy)
import Statistics.Test.Types


-- | The Wilcoxon Rank Sums Test.
--
-- This test calculates the sum of ranks for the given two samples.  The samples
-- are ordered, and assigned ranks (ties are given their average rank), then these
-- ranks are summed for each sample.
--
-- The return value is (W&#8321;, W&#8322;) where W&#8321; is the sum of ranks of the first sample
-- and W&#8322; is the sum of ranks of the second sample.  This test is trivially transformed
-- into the Mann-Whitney U test.  You will probably want to use 'mannWhitneyU'
-- and the related functions for testing significance, but this function is exposed
-- for completeness.
wilcoxonRankSums :: Sample -> Sample -> (Double, Double)
wilcoxonRankSums xs1 xs2 = ( U.sum ranks1 , U.sum ranks2
                           )
  where
    -- Ranks for each sample
    (ranks1,ranks2) = splitByTags $ U.zip tags (rank (==) joinSample)
    -- Sorted and tagged sample
    (tags,joinSample) = U.unzip
                      $ sortBy (comparing snd)
                      $ tagSample True xs1 U.++ tagSample False xs2
    -- Add tag to a sample
    tagSample t = U.map ((,) t)



-- | The Mann-Whitney U Test.
--
-- This is sometimes known as the Mann-Whitney-Wilcoxon U test, and
-- confusingly many sources state that the Mann-Whitney U test is the same as
-- the Wilcoxon's rank sum test (which is provided as 'wilcoxonRankSums').
-- The Mann-Whitney U is a simple transform of Wilcoxon's rank sum test.
--
-- Again confusingly, different sources state reversed definitions for U&#8321;
-- and U&#8322;, so it is worth being explicit about what this function returns.
-- Given two samples, the first, xs&#8321;, of size n&#8321; and the second, xs&#8322;,
-- of size n&#8322;, this function returns (U&#8321;, U&#8322;)
-- where U&#8321; = W&#8321; - (n&#8321;(n&#8321;+1))\/2
-- and U&#8322; = W&#8322; - (n&#8322;(n&#8322;+1))\/2,
-- where (W&#8321;, W&#8322;) is the return value of @wilcoxonRankSums xs1 xs2@.
--
-- Some sources instead state that U&#8321; and U&#8322; should be the other way round, often
-- expressing this using U&#8321;' = n&#8321;n&#8322; - U&#8321; (since U&#8321; + U&#8322; = n&#8321;n&#8322;).
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

-- | Calculates the critical value of Mann-Whitney U for the given sample
-- sizes and significance level.
--
-- This function returns the exact calculated value of U for all sample sizes;
-- it does not use the normal approximation at all.  Above sample size 20 it is
-- generally recommended to use the normal approximation instead, but this function
-- will calculate the higher critical values if you need them.
--
-- The algorithm to generate these values is a faster, memoised version of the
-- simple unoptimised generating function given in section 2 of \"The Mann Whitney
-- Wilcoxon Distribution Using Linked Lists\", Cheung and Klotz, Statistica Sinica
-- 7 (1997), <http://www3.stat.sinica.edu.tw/statistica/oldpdf/A7n316.pdf>.
mannWhitneyUCriticalValue :: (Int, Int) -- ^ The sample size
                          -> Double     -- ^ The p-value (e.g. 0.05) for which you want the critical value.
                          -> Maybe Int  -- ^ The critical value (of U).
mannWhitneyUCriticalValue (m, n) p
  | m < 1 || n < 1 = Nothing    -- Sample must be nonempty
  | p  >= 1        = Nothing    -- Nonsensical p-value
  | p' <= 1        = Nothing    -- p-value is too small. Null hypothesys couln't be disproved
  | otherwise      = findIndex (>= p')
                   $ take (m*n)
                   $ tail
                   $ alookup !! (m+n-2) !! (min m n - 1)
  where
    mnCn = (m+n) `choose` n
    p'   = mnCn * p


{-
-- Original function, without memoisation, from Cheung and Klotz:
-- Double is needed to avoid integer overflows.
a :: Int -> Int -> Int -> Double
a u bigN m
  | u < 0            = 0
  | u >= m * n       = bigN `choose` m
  | m == 1 || n == 1 = fromIntegral (u + 1)
  | otherwise        = a  u      (bigN - 1)  m
                     + a (u - n) (bigN - 1) (m-1)
  where
    n = bigN - m
-}

-- Memoised version of the original a function, above. 
--
-- Doubles are stored to avoid integer overflow. 32-bit Ints begin to
-- overflow for bigN as small as 33 (64-bit one at 66) while Double to
-- go to infinity till bigN=1029
-- 
--
-- outer list is indexed by big N - 2
-- inner list by (m-1) (we know m < bigN)
-- innermost list by u
--
-- So: (alookup !! (bigN - 2) !! (m - 1) ! u) == a u bigN m
alookup :: [[[Double]]]
alookup = gen 2 [1 : repeat 2]
  where
    gen bigN predBigNList
       = let bigNlist = [ [ amemoed u m
                          | u <- [0 .. m*(bigN-m)]
                          ] ++ repeat (bigN `choose` m)
                        | m <- [1 .. (bigN-1)]] -- has bigN-1 elements
         in bigNlist : gen (bigN+1) bigNlist
      where
        amemoed :: Int -> Int -> Double
        amemoed u m
          | m == 1 || n == 1 = fromIntegral (u + 1)
          | otherwise        = mList !! u
                             + if u < n then 0 else predmList !! (u-n)
          where
            n = bigN - m
            (predmList : mList : _) = drop (m-2) predBigNList
            -- Lists for m-1 and m respectively. i-th list correspond to m=i+1
            --
            -- We know that predBigNList has bigN - 2 elements
            -- (and we know that n > 1 therefore bigN > m + 1)
            -- So bigN - 2 >= m, i.e. predBigNList must have at least m elements
            -- elements, so dropping (m-2) must leave at least 2


-- | Calculates whether the Mann Whitney U test is significant.
--
-- If both sample sizes are less than or equal to 20, the exact U critical value
-- (as calculated by 'mannWhitneyUCriticalValue') is used.  If either sample is
-- larger than 20, the normal approximation is used instead.
--
-- If you use a one-tailed test, the test indicates whether the first sample is
-- significantly larger than the second.  If you want the opposite, simply reverse
-- the order in both the sample size and the (U&#8321;, U&#8322;) pairs.
mannWhitneyUSignificant ::
     TestType         -- ^ Perform one-tailed test (see description above).
  -> (Int, Int)       -- ^ The samples' size from which the (U&#8321;,U&#8322;) values were derived.
  -> Double           -- ^ The p-value at which to test (e.g. 0.05)
  -> (Double, Double) -- ^ The (U&#8321;, U&#8322;) values from 'mannWhitneyU'.
  -> Maybe TestResult -- ^ Return 'Nothing' if the sample was too
                      --   small to make a decision.
mannWhitneyUSignificant test (in1, in2) p (u1, u2)
   --Use normal approximation
  | in1 > 20 || in2 > 20 =
    let mean  = n1 * n2 / 2
        sigma = sqrt $ n1*n2*(n1 + n2 + 1) / 12
        z     = (mean - u1) / sigma
    in Just $ case test of
                OneTailed -> significant $ z     < quantile standard  p
                TwoTailed -> significant $ abs z > abs (quantile standard (p/2))
  -- Use exact critical value
  | otherwise = do crit <- fromIntegral <$> mannWhitneyUCriticalValue (in1, in2) p
                   return $ case test of
                              OneTailed -> significant $ u2        <= crit
                              TwoTailed -> significant $ min u1 u2 <= crit
  where
    n1 = fromIntegral in1
    n2 = fromIntegral in2


-- | Perform Mann-Whitney U Test for two samples and required
-- significance. For additional information check documentation of
-- 'mannWhitneyU' and 'mannWhitneyUSignificant'. This is just a helper
-- function.
--
-- One-tailed test checks whether first sample is significantly larger
-- than second. Two-tailed whether they are significantly different.
mannWhitneyUtest :: TestType    -- ^ Perform one-tailed test (see description above).
                 -> Double      -- ^ The p-value at which to test (e.g. 0.05)
                 -> Sample      -- ^ First sample
                 -> Sample      -- ^ Second sample
                 -> Maybe TestResult
                 -- ^ Return 'Nothing' if the sample was too small to
                 --   make a decision.
mannWhitneyUtest ontTail p smp1 smp2 =
  mannWhitneyUSignificant ontTail (n1,n2) p $ mannWhitneyU smp1 smp2
    where
      n1 = U.length smp1
      n2 = U.length smp2

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
     TestType            -- ^ Perform one-tailed test (see description above).
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


----------------------------------------------------------------
-- Helpers

-- Private data type for unfolding
data Rank v a = Rank { rankCnt :: Int        -- Number of ranks to return
                     , rankVal :: Double     -- Rank to return
                     , rankNum :: Double     -- Current rank
                     , rankVec :: v a        -- Remaining vector
                     }

-- Calculate rank of sample. Sample should be already sorted
rank :: (G.Vector v a, G.Vector v Double, Eq a)
     => (a -> a -> Bool)        -- Equivalence relation
     -> v a                     -- Vector to rank
     -> v Double
rank eq vec = G.unfoldr go (Rank 0 (-1) 1 vec)
  where
    go (Rank 0 _ r v)
      | G.null v  = Nothing
      | otherwise =
          case G.length h of
            1 -> Just (r, Rank 0 0 (r+1) rest)
            n -> go $ Rank { rankCnt = n
                           , rankVal = 0.5 * (r*2 + fromIntegral (n-1))
                           , rankNum = r + fromIntegral n
                           , rankVec = rest
                           }
          where
            (h,rest) = G.span (eq $ G.head v) v
    go (Rank n val r v) = Just (val, Rank (n-1) val r v)
{-# INLINE rank #-}


-- Split tagged vector
splitByTags :: (G.Vector v a, G.Vector v (Bool,a)) => v (Bool,a) -> (v a, v a)
splitByTags vs = (G.map snd a, G.map snd b)
  where
    (a,b) = G.unstablePartition fst vs
{-# INLINE splitByTags #-}
