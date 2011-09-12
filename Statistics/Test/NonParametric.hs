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
module Statistics.Test.NonParametric
  ( -- * Mann-Whitney U test (non-parametric equivalent to the independent t-test)
    mannWhitneyU
  , mannWhitneyUCriticalValue
  , mannWhitneyUSignificant
   -- * Wilcoxon signed-rank matched-pair test (non-parametric equivalent to the paired t-test)
  , wilcoxonMatchedPairSignedRank
  , wilcoxonMatchedPairSignificant
  , wilcoxonMatchedPairSignificance
  , wilcoxonMatchedPairCriticalValue
  -- * Wilcoxon rank sum test
  , wilcoxonRankSums
  ) where

import Control.Applicative ((<$>))
import Control.Arrow       ((***))
import Data.Function       (on)
import Data.List           (findIndex, groupBy, partition, sortBy)
import Data.Ord            (comparing)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

import Statistics.Distribution        (quantile)
import Statistics.Distribution.Normal (standard)
import Statistics.Math                (choose)
import Statistics.Types               (Sample)
import qualified Statistics.Function as SF

type AbsoluteRank = Double
type SignedRank   = Double

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
wilcoxonRankSums xs1 xs2 = ( U.sum $ U.map snd ranks1
                           , U.sum $ U.map snd ranks2
                           )
  where
    -- Ranks for each sample
    (ranks1,ranks2) = U.unstablePartition fst $ U.zip tags (rank joinSample)
    -- Sorted and tagged sample
    (tags,joinSample) = U.unzip
                      $ SF.sortBy (comparing snd)
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
-- Again confusingly, different sources state reversed definitions for U_1 and U_2,
-- so it is worth being explicit about what this function returns.  Given two samples,
-- the first, xs_1, of size n_1 and the second, xs_2, of size n_2, this function
-- returns (U_1, U_2) where U_1 = W_1 - (n_1*(n_1+1))\/2 and U_2 = W_2 - (n_2*(n_2+1))\/2,
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
  | p' <= 1 = Nothing
  | m < 1 || n < 1 = Nothing
  | otherwise = findIndex (>= p') $ let
     firstHalf = map fromIntegral $ take (((m*n)+1)`div`2) $ tail $ alookup !! (m+n-2) !! (min m n - 1)
       {- Original: [fromIntegral $ a k (m+n) (min m n) | k <- [1..m*n]] -}
     secondHalf
       | even (m*n) = reverse firstHalf
       | otherwise = tail $ reverse firstHalf
     in firstHalf ++ map (mnCn -) secondHalf
  where
    mnCn = (m+n) `choose` n
    p' = mnCn * p

{- Original function, without memoisation, from Cheung and Klotz:
a :: Int -> Int -> Int -> Int
a u bigN m
      | u < 0 = 0
      | u >= (m * smalln) = floor $ fromIntegral bigN `choose` fromIntegral m
      | m == 1 || smalln == 1 = u + 1
      | otherwise = a u (bigN - 1) m
                  + a (u - smalln) (bigN - 1) (m-1)
  where smalln = bigN - m
-}

-- Memoised version of the original a function, above.
--
-- outer list is indexed by big N - 2
-- inner list by m (we know m < bigN)
-- innermost list by u
--
-- So: (alookup ! (bigN - 2) ! m ! u) == a u bigN m
alookup :: [[[Int]]]
alookup = gen 2 [1 : repeat 2]
  where
    gen bigN predBigNList
       = let bigNlist = [ let limit = round $ fromIntegral bigN `choose` fromIntegral m
                          in [amemoed u m | u <- [0..m*(bigN-m)]] ++ repeat limit
                        | m <- [1..(bigN-1)]] -- has bigN-1 elements
         in bigNlist : gen (bigN+1) bigNlist
      where
        amemoed :: Int -> Int -> Int
        amemoed u m
          | m == 1 || smalln == 1 = u + 1
          | otherwise = let (predmList : mList : _) = drop (m-2) predBigNList -- m-2 because starts at 1
                        -- We know that predBigNList has bigN - 2 elements
                        -- (and we know that smalln > 1 therefore bigN > m + 1)
                        -- So bigN - 2 >= m, i.e. predBigNList must have at least m elements
                        -- elements, so dropping (m-2) must leave at least 2
                        in (mList !! u) + (if u < smalln then 0 else predmList !! (u - smalln))
          where smalln = bigN - m

-- | Calculates whether the Mann Whitney U test is significant.
--
-- If both sample sizes are less than or equal to 20, the exact U critical value
-- (as calculated by 'mannWhitneyUCriticalValue') is used.  If either sample is
-- larger than 20, the normal approximation is used instead.
--
-- If you use a one-tailed test, the test indicates whether the first sample is
-- significantly larger than the second.  If you want the opposite, simply reverse
-- the order in both the sample size and the (U_1, U_2) pairs.
mannWhitneyUSignificant ::
     Bool             -- ^ Perform one-tailed test (see description above).
  -> (Int, Int)       -- ^ The sample size from which the (U_1,U_2) values were derived.
  -> Double           -- ^ The p-value at which to test (e.g. 0.05)
  -> (Double, Double) -- ^ The (U_1, U_2) values from 'mannWhitneyU'.
  -> Maybe Bool       -- ^ Just True if the test is significant, Just
                      --   False if it is not, and Nothing if the sample
                      --   was too small to make a decision.
mannWhitneyUSignificant oneTail (in1, in2) p (u1, u2)
  | in1 > 20 || in2 > 20 --Use normal approximation
--     = (n1*(n1+1))/2 - u1 - (n1*(n1+n2))/2
--     = (n1*(n1+1))/2 - (-2*u1 + n1*(n1+n2))/2
--     = (n1*(n1+1) - 2*u1 + n1*(n1+n2))/2
--     = (n1*(2*n1 + n2 + 1) - 2*u1)/2
       = let num = (n1*(2*n1 + n2 + 1)) / 2 - u1
             denom = sqrt $ n1*n2*(n1 + n2 + 1) / 12
             z = num / denom
             zcrit = quantile standard (1 - if oneTail then p else p/2)
         in Just $ (if oneTail then z else abs z) > zcrit
  | otherwise = do crit <- fromIntegral <$> mannWhitneyUCriticalValue (in1, in2) p
                   return $ if oneTail
                              then u2 <= crit
                              else min u1 u2 <= crit
  where
    n1 = fromIntegral in1
    n2 = fromIntegral in2

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
wilcoxonMatchedPairSignificant ::
     Bool                -- ^ Perform one-tailed test (see description above).
  -> Int                 -- ^ The sample size from which the (T+,T-) values were derived.
  -> Double              -- ^ The p-value at which to test (e.g. 0.05)
  -> (Double, Double)    -- ^ The (T+, T-) values from 'wilcoxonMatchedPairSignedRank'.
  -> Maybe Bool          -- ^ Just True if the test is significant, Just
                         --   False if it is not, and Nothing if the sample
                         --   was too small to make a decision.
wilcoxonMatchedPairSignificant oneTail sampleSize p (tPlus, tMinus)
  -- According to my nearest book (Understanding Research Methods and Statistics
  -- by Gary W. Heiman, p590), to check that the first sample is bigger you must
  -- use the absolute value of T- for a one-tailed check:
  | oneTail = ((abs tMinus <=) . fromIntegral) <$> wilcoxonMatchedPairCriticalValue sampleSize p
  -- Otherwise you must use the value of T+ and T- with the smallest absolute value:
  | otherwise = ((t <=) . fromIntegral) <$> wilcoxonMatchedPairCriticalValue sampleSize (p/2)
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
wilcoxonMatchedPairSignificance sampleSize rank
  = (summedCoefficients sampleSize !! floor rank) / 2 ** fromIntegral sampleSize


----------------------------------------------------------------
-- Helpers

-- Private data type for unfolding
data Rank v a = Rank { rankCnt :: Int        -- Number of ranks to return
                     , rankVal :: Double     -- Rank to return
                     , rankNum :: Double     -- Current rank
                     , rankVec :: v a        -- Remaining vector
                     }

-- Calculate rank of sample. Sample should be already sorted
rank :: (G.Vector v a, G.Vector v Double, Eq a) => v a -> v Double
rank vec = G.unfoldr go (Rank 0 (-1) 1 vec)
  where
    go (Rank 0 _ r v)
      | G.null v  = Nothing
      | otherwise =
          case G.length h of
            1 -> Just (r, (Rank 0 0 (r+1) rest))
            n -> go $ Rank { rankCnt = n
                           , rankVal = 0.5 * (r*2 + fromIntegral (n-1))
                           , rankNum = r + fromIntegral n
                           , rankVec = rest
                           }
          where
            (h,rest) = G.span (== (G.head v)) v
    go (Rank n val r v) = Just (val, (Rank (n-1) val r v))
{-# INLINE rank #-}
