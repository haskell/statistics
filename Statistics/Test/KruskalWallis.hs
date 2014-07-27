-- |
-- Module    : Statistics.Test.KruskalWallis
-- Copyright : (c) 2014 Danny Navarro
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
module Statistics.Test.KruskalWallis
  ( kruskalWallisRank
  , kruskalWallis
  , kruskalWallisSignificant
  , kruskalWallisTest
  ) where

import Data.Ord (comparing)
import Data.Foldable (foldMap)
import qualified Data.Vector.Unboxed as U
import Statistics.Function (sort, sortBy, square)
import Statistics.Distribution (quantile)
import Statistics.Distribution.ChiSquared (chiSquared)
import Statistics.Test.Types (TestResult(..), significant)
import Statistics.Test.Internal (rank)
import Statistics.Sample
import qualified Statistics.Sample.Internal as Sample(sum)


-- | Kruskal-Wallis ranking.
--
-- All values are replaced by the absolute rank in the combined samples.
--
-- The samples and values need not to be ordered but the values in the result
-- are ordered. Assigned ranks (ties are given their average rank).
kruskalWallisRank :: (U.Unbox a, Ord a) => [U.Vector a] -> [Sample]
kruskalWallisRank samples = groupByTags
                          . sortBy (comparing fst)
                          . U.zip tags
                          $ rank (==) joinSample
  where
    (tags,joinSample) = U.unzip
                      . sortBy (comparing snd)
                      $ foldMap (uncurry tagSample) $ zip [(1::Int)..] samples
    tagSample t = U.map (\x -> (t,x))

    groupByTags xs
        | U.null xs = []
        | otherwise = sort (U.map snd ys) : groupByTags zs
      where
        (ys,zs) = U.span ((==) (fst $ U.head xs) . fst) xs


-- | The Kruskal-Wallis Test.
--
-- In textbooks the output value is usually represented by 'K' or 'H'. This
-- function already does the ranking.
kruskalWallis :: (U.Unbox a, Ord a) => [U.Vector a] -> Double
kruskalWallis samples = (nTot - 1) * numerator / denominator
  where
    -- Total number of elements in all samples
    nTot    = fromIntegral $ sumWith rsamples U.length
    -- Average rank of all samples
    avgRank = (nTot + 1) / 2
    --
    numerator = sumWith rsamples $ \sample ->
        let n = fromIntegral $ U.length sample
        in  n * square (mean sample - avgRank)
    denominator = sumWith rsamples $ \sample ->
        Sample.sum $ U.map (\r -> square (r - avgRank)) sample

    rsamples = kruskalWallisRank samples


-- | Calculates whether the Kruskal-Wallis test is significant.
--
-- It uses /Chi-Squared/ distribution for aproximation as long as the sizes are
-- larger than 5. Otherwise the test returns 'Nothing'.
kruskalWallisSignificant ::
       [Int]  -- ^ The samples' size
    -> Double -- ^ The p-value at which to test (e.g. 0.05)
    -> Double -- ^ K value from 'kruskallWallis'
    -> Maybe TestResult
kruskalWallisSignificant ns p k
    -- Use chi-squared approximation
    | all (>4) ns = Just . significant $ k > x
    -- TODO: Implement critical value calculation: kruskalWallisCriticalValue
    | otherwise = Nothing
  where
    x = quantile (chiSquared (length ns - 1)) (1 - p)

-- | Perform Kruskal-Wallis Test for the given samples and required
-- significance. For additional information check 'kruskalWallis'. This is just
-- a helper function.
kruskalWallisTest :: Double -> [Sample] -> Maybe TestResult
kruskalWallisTest p samples =
    kruskalWallisSignificant (map U.length samples) p $ kruskalWallis samples

-- * Helper functions

sumWith :: Num a => [Sample] -> (Sample -> a) -> a
sumWith samples f = Prelude.sum $ fmap f samples
{-# INLINE sumWith #-}
