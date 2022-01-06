{-# LANGUAGE ViewPatterns #-}
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
-- which could be used to test whether two related samples have
-- different means.
module Statistics.Test.WilcoxonT (
    -- * Wilcoxon signed-rank matched-pair test
    -- ** Test
    wilcoxonMatchedPairTest
    -- ** Building blocks
  , wilcoxonMatchedPairSignedRank
  , wilcoxonMatchedPairSignificant
  , wilcoxonMatchedPairSignificance
  , wilcoxonMatchedPairCriticalValue
  , module Statistics.Test.Types
    -- * References
    -- $references
  ) where



--
--
--
-- Note that: wilcoxonMatchedPairSignedRank == (\(x, y) -> (y, x)) . flip wilcoxonMatchedPairSignedRank
-- The samples are zipped together: if one is longer than the other, both are truncated
-- The value returned is the pair (T+, T-).  T+ is the sum of positive ranks (the
-- These values mean little by themselves, and should be combined with the 'wilcoxonSignificant'
-- function in this module to get a meaningful result.
-- ranks of the differences where the first parameter is higher) whereas T- is
-- the sum of negative ranks (the ranks of the differences where the second parameter is higher).
-- to the length of the shorter sample.

import Data.Function (on)
import Data.List (findIndex)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import Prelude hiding (sum)
import Statistics.Function (sortBy)
import Statistics.Sample.Internal (sum)
import Statistics.Test.Internal (rank, splitByTags)
import Statistics.Test.Types
import Statistics.Types -- (CL,pValue,getPValue)
import Statistics.Distribution
import Statistics.Distribution.Normal


-- | Calculate (n,T⁺,T⁻) values for both samples. Where /n/ is reduced
--   sample where equal pairs are removed.
wilcoxonMatchedPairSignedRank :: (Ord a, Num a, U.Unbox a) => U.Vector (a,a) -> (Int, Double, Double)
wilcoxonMatchedPairSignedRank ab
  = (nRed, sum ranks1, negate (sum ranks2))
  where
    -- Positive and negative ranks
    (ranks1, ranks2) = splitByTags
                     $ U.zip tags (rank ((==) `on` abs) diffs)
    -- Sorted list of differences
    diffsSorted = sortBy (comparing abs)    -- Sort the differences by absolute difference
                $ U.filter  (/= 0)          -- Remove equal elements
                $ U.map (uncurry (-)) ab    -- Work out differences
    nRed = U.length diffsSorted
    -- Sign tags and differences
    (tags,diffs) = U.unzip
                 $ U.map (\x -> (x>0 , x))   -- Attach tags to distribution elements
                 $ diffsSorted



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
wilcoxonMatchedPairSignificant
  :: PositionTest          -- ^ How to compare two samples
  -> PValue Double         -- ^ The p-value at which to test (e.g. @mkPValue 0.05@)
  -> (Int, Double, Double) -- ^ The (n,T⁺, T⁻) values from 'wilcoxonMatchedPairSignedRank'.
  -> Maybe TestResult      -- ^ Return 'Nothing' if the sample was too
                           --   small to make a decision.
wilcoxonMatchedPairSignificant test pVal (sampleSize, tPlus, tMinus) =
  case test of
    -- According to my nearest book (Understanding Research Methods and Statistics
    -- by Gary W. Heiman, p590), to check that the first sample is bigger you must
    -- use the absolute value of T- for a one-tailed check:
    AGreater      -> do crit <- wilcoxonMatchedPairCriticalValue sampleSize pVal
                        return $ significant $ abs tMinus <= fromIntegral crit
    BGreater      -> do crit <- wilcoxonMatchedPairCriticalValue sampleSize pVal
                        return $ significant $ abs tPlus <= fromIntegral crit
    -- Otherwise you must use the value of T+ and T- with the smallest absolute value:
    --
    -- Note that in absence of ties sum of |T+| and |T-| is constant
    -- so by selecting minimal we are performing two-tailed test and
    -- look and both tails of distribution of T.
    SamplesDiffer -> do crit <- wilcoxonMatchedPairCriticalValue sampleSize (mkPValue $ p/2)
                        return $ significant $ t <= fromIntegral crit
  where
    t = min (abs tPlus) (abs tMinus)
    p = pValue pVal


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
-- The return values of this function are generated using the method
-- detailed in the Mitic's paper. According to that paper, the results
-- may differ from other published lookup tables, but (Mitic claims)
-- the values obtained by this function will be the correct ones.
wilcoxonMatchedPairCriticalValue ::
     Int                -- ^ The sample size
  -> PValue Double      -- ^ The p-value (e.g. @mkPValue 0.05@) for which you want the critical value.
  -> Maybe Int          -- ^ The critical value (of T), or Nothing if
                        --   the sample is too small to make a decision.
wilcoxonMatchedPairCriticalValue n pVal
  | n < 100   =
      case subtract 1 <$> findIndex (> m) (summedCoefficients n) of
        Just k | k < 0     -> Nothing
               | otherwise -> Just k
        Nothing  -> error "Statistics.Test.WilcoxonT.wilcoxonMatchedPairCriticalValue: impossible happened"
  | otherwise =
     case quantile (normalApprox n) p of
       z | z < 0     -> Nothing
         | otherwise -> Just (round z)
  where
    p = pValue pVal
    m = (2 ** fromIntegral n) * p


-- | Works out the significance level (p-value) of a T value, given a sample
-- size and a T value from the Wilcoxon signed-rank matched-pairs test.
--
-- See the notes on 'wilcoxonCriticalValue' for how this is calculated.
wilcoxonMatchedPairSignificance
  :: Int           -- ^ The sample size
  -> Double        -- ^ The value of T for which you want the significance.
  -> PValue Double -- ^ The significance (p-value).
wilcoxonMatchedPairSignificance n t
  = mkPValue p
  where
    p | n < 100   = (summedCoefficients n !! floor t) / 2 ** fromIntegral n
      | otherwise = cumulative (normalApprox n) t


-- | Normal approximation for Wilcoxon T statistics
normalApprox :: Int -> NormalDistribution
normalApprox ni
  = normalDistr m s
  where
    m = n * (n + 1) / 4
    s = sqrt $ (n * (n + 1) * (2*n + 1)) / 24
    n = fromIntegral ni


-- | The Wilcoxon matched-pairs signed-rank test. The samples are
-- zipped together: if one is longer than the other, both are
-- truncated to the length of the shorter sample.
--
-- For one-tailed test it tests whether first sample is significantly
-- greater than the second. For two-tailed it checks whether they
-- significantly differ
--
-- Check 'wilcoxonMatchedPairSignedRank' and
-- 'wilcoxonMatchedPairSignificant' for additional information.
wilcoxonMatchedPairTest
  :: (Ord a, Num a, U.Unbox a)
  => PositionTest     -- ^ Perform one-tailed test.
  -> U.Vector (a,a)   -- ^ Sample of pairs
  -> Test ()          -- ^ Return 'Nothing' if the sample was too
                      --   small to make a decision.
wilcoxonMatchedPairTest test pairs =
  Test { testSignificance = pVal
       , testStatistics   = t
       , testDistribution = ()
       }
  where
    (n,tPlus,tMinus) = wilcoxonMatchedPairSignedRank pairs
    (t,pVal) = case test of
                 AGreater      -> (abs tMinus, wilcoxonMatchedPairSignificance n (abs tMinus))
                 BGreater      -> (abs tPlus,  wilcoxonMatchedPairSignificance n (abs tPlus ))
                 -- Since we take minimum of T+,T- we can't get more
                 -- that p=0.5 and can multiply it by 2 without risk
                 -- of error.
                 SamplesDiffer -> let t' = min (abs tMinus) (abs tPlus)
                                      p  = wilcoxonMatchedPairSignificance n t'
                                  in (t', mkPValue $ min 1 $ 2 * pValue p)


-- $references
--
-- * \"Critical Values for the Wilcoxon Signed Rank Statistic\", Peter
--   Mitic, The Mathematica Journal, volume 6, issue 3, 1996
--   (<http://www.mathematica-journal.com/issue/v6i3/article/mitic/contents/63mitic.pdf>)
