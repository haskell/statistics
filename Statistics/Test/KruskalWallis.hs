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
  ( -- * Kruskal-Wallis test
    kruskalWallisTest
    -- ** Building blocks
  , kruskalWallisRank
  , kruskalWallis
  , module Statistics.Test.Types
  ) where

import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import Statistics.Function (sort, sortBy, square)
import Statistics.Distribution (complCumulative)
import Statistics.Distribution.ChiSquared (chiSquared)
import Statistics.Types
import Statistics.Test.Types
import Statistics.Test.Internal (rank)
import Statistics.Sample
import qualified Statistics.Sample.Internal as Sample(sum)


-- | Kruskal-Wallis ranking.
--
-- All values are replaced by the absolute rank in the combined samples.
--
-- The samples and values need not to be ordered but the values in the result
-- are ordered. Assigned ranks (ties are given their average rank).
kruskalWallisRank :: (U.Unbox a, Ord a) => [U.Vector a] -> [U.Vector Double]
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


-- | Perform Kruskal-Wallis Test for the given samples and required
-- significance. For additional information check 'kruskalWallis'. This is just
-- a helper function.
--
-- It uses /Chi-Squared/ distribution for approximation as long as the sizes are
-- larger than 5. Otherwise the test returns 'Nothing'.
kruskalWallisTest :: (Ord a, U.Unbox a) => [U.Vector a] -> Maybe (Test ())
kruskalWallisTest []      = Nothing
kruskalWallisTest samples
  -- We use chi-squared approximation here
  | all (>4) ns = Just Test { testSignificance = mkPValue $ complCumulative d k
                            , testStatistics   = k
                            , testDistribution = ()
                            }
  | otherwise   = Nothing
  where
    k  = kruskalWallis samples
    ns = map U.length samples
    d  = chiSquared (length ns - 1)

-- * Helper functions

sumWith :: Num a => [Sample] -> (Sample -> a) -> a
sumWith samples f = Prelude.sum $ fmap f samples
{-# INLINE sumWith #-}
