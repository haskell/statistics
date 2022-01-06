-- |
-- Module    : Statistics.Test.MannWhitneyU
-- Copyright : (c) 2010 Neil Brown
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Mann-Whitney U test (also know as Mann-Whitney-Wilcoxon and
-- Wilcoxon rank sum test) is a non-parametric test for assessing
-- whether two samples of independent observations have different
-- mean.
module Statistics.Test.MannWhitneyU (
    -- * Mann-Whitney U test
    mannWhitneyUtest
  , mannWhitneyU
  , mannWhitneyUCriticalValue
  , mannWhitneyUSignificant
    -- ** Wilcoxon rank sum test
  , wilcoxonRankSums
  , module Statistics.Test.Types
    -- * References
    -- $references
  ) where

import Data.List (findIndex)
import Data.Ord (comparing)
import Numeric.SpecFunctions (choose)
import Prelude hiding (sum)
import Statistics.Distribution (quantile)
import Statistics.Distribution.Normal (standard)
import Statistics.Function (sortBy)
import Statistics.Sample.Internal (sum)
import Statistics.Test.Internal (rank, splitByTags)
import Statistics.Test.Types (TestResult(..), PositionTest(..), significant)
import Statistics.Types (PValue,pValue)
import qualified Data.Vector.Unboxed as U

-- | The Wilcoxon Rank Sums Test.
--
-- This test calculates the sum of ranks for the given two samples.
-- The samples are ordered, and assigned ranks (ties are given their
-- average rank), then these ranks are summed for each sample.
--
-- The return value is (W₁, W₂) where W₁ is the sum of ranks of the first sample
-- and W₂ is the sum of ranks of the second sample.  This test is trivially transformed
-- into the Mann-Whitney U test.  You will probably want to use 'mannWhitneyU'
-- and the related functions for testing significance, but this function is exposed
-- for completeness.
wilcoxonRankSums :: (Ord a, U.Unbox a) => U.Vector a -> U.Vector a -> (Double, Double)
wilcoxonRankSums xs1 xs2 = (sum ranks1, sum ranks2)
  where
    -- Ranks for each sample
    (ranks1,ranks2) = splitByTags $ U.zip tags (rank (==) joinSample)
    -- Sorted and tagged sample
    (tags,joinSample) = U.unzip
                      $ sortBy (comparing snd)
                      $ tagSample True xs1 U.++ tagSample False xs2
    -- Add tag to a sample
    tagSample t = U.map (\x -> (t,x))



-- | The Mann-Whitney U Test.
--
-- This is sometimes known as the Mann-Whitney-Wilcoxon U test, and
-- confusingly many sources state that the Mann-Whitney U test is the same as
-- the Wilcoxon's rank sum test (which is provided as 'wilcoxonRankSums').
-- The Mann-Whitney U is a simple transform of Wilcoxon's rank sum test.
--
-- Again confusingly, different sources state reversed definitions for U₁
-- and U₂, so it is worth being explicit about what this function returns.
-- Given two samples, the first, xs₁, of size n₁ and the second, xs₂,
-- of size n₂, this function returns (U₁, U₂)
-- where U₁ = W₁ - (n₁(n₁+1))\/2
-- and U₂ = W₂ - (n₂(n₂+1))\/2,
-- where (W₁, W₂) is the return value of @wilcoxonRankSums xs1 xs2@.
--
-- Some sources instead state that U₁ and U₂ should be the other way round, often
-- expressing this using U₁' = n₁n₂ - U₁ (since U₁ + U₂ = n₁n₂).
--
-- All of which you probably don't care about if you just feed this into 'mannWhitneyUSignificant'.
mannWhitneyU :: (Ord a, U.Unbox a) => U.Vector a -> U.Vector a -> (Double, Double)
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
-- Wilcoxon Distribution Using Linked Lists\"
mannWhitneyUCriticalValue
  :: (Int, Int)     -- ^ The sample size
  -> PValue Double  -- ^ The p-value (e.g. 0.05) for which you want the critical value.
  -> Maybe Int      -- ^ The critical value (of U).
mannWhitneyUCriticalValue (m, n) p
  | m < 1 || n < 1 = Nothing    -- Sample must be nonempty
  | p' <= 1        = Nothing    -- p-value is too small. Null hypothesis couldn't be disproved
  | otherwise      = findIndex (>= p')
                   $ take (m*n)
                   $ tail
                   $ alookup !! (m+n-2) !! (min m n - 1)
  where
    mnCn = (m+n) `choose` n
    p'   = mnCn * pValue p


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
-- the order in both the sample size and the (U₁, U₂) pairs.
mannWhitneyUSignificant
  :: PositionTest     -- ^ Perform one-tailed test (see description above).
  -> (Int, Int)       -- ^ The samples' size from which the (U₁,U₂) values were derived.
  -> PValue Double    -- ^ The p-value at which to test (e.g. 0.05)
  -> (Double, Double) -- ^ The (U₁, U₂) values from 'mannWhitneyU'.
  -> Maybe TestResult -- ^ Return 'Nothing' if the sample was too
                      --   small to make a decision.
mannWhitneyUSignificant test (in1, in2) pVal (u1, u2)
  -- Use normal approximation
  | in1 > 20 || in2 > 20 =
    let mean  = n1 * n2 / 2     -- (u1+u2) / 2
        sigma = sqrt $ n1*n2*(n1 + n2 + 1) / 12
        z     = (mean - u1) / sigma
    in Just $ case test of
                AGreater      -> significant $ z     < quantile standard p
                BGreater      -> significant $ (-z)  < quantile standard p
                SamplesDiffer -> significant $ abs z > abs (quantile standard (p/2))
  -- Use exact critical value
  | otherwise = do crit <- fromIntegral <$> mannWhitneyUCriticalValue (in1, in2) pVal
                   return $ case test of
                              AGreater      -> significant $ u2        <= crit
                              BGreater      -> significant $ u1        <= crit
                              SamplesDiffer -> significant $ min u1 u2 <= crit
  where
    n1 = fromIntegral in1
    n2 = fromIntegral in2
    p  = pValue pVal


-- | Perform Mann-Whitney U Test for two samples and required
-- significance. For additional information check documentation of
-- 'mannWhitneyU' and 'mannWhitneyUSignificant'. This is just a helper
-- function.
--
-- One-tailed test checks whether first sample is significantly larger
-- than second. Two-tailed whether they are significantly different.
mannWhitneyUtest
  :: (Ord a, U.Unbox a)
  => PositionTest     -- ^ Perform one-tailed test (see description above).
  -> PValue Double    -- ^ The p-value at which to test (e.g. 0.05)
  -> U.Vector a       -- ^ First sample
  -> U.Vector a       -- ^ Second sample
  -> Maybe TestResult -- ^ Return 'Nothing' if the sample was too small to
                      --   make a decision.
mannWhitneyUtest ontTail p smp1 smp2 =
  mannWhitneyUSignificant ontTail (n1,n2) p $ mannWhitneyU smp1 smp2
    where
      n1 = U.length smp1
      n2 = U.length smp2

-- $references
--
-- * Cheung, Y.K.; Klotz, J.H. (1997) The Mann Whitney Wilcoxon
--   distribution using linked lists. /Statistica Sinica/
--   7:805&#8211;813.
-- <http://www3.stat.sinica.edu.tw/statistica/oldpdf/A7n316.pdf>.
