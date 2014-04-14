module Statistics.Test.KruskalWallis
  ( kruskalWallisRank
  , kruskalWallis
  ) where

import Data.Ord (comparing)
import Data.Foldable (foldMap)
import qualified Data.Vector.Unboxed as U
import Statistics.Types (Sample)
import Statistics.Function (sort, sortBy)
import Statistics.Test.Internal (rank)
import qualified Statistics.Sample.Internal as Sample(sum)

kruskalWallisRank :: [Sample] -> [Sample]
kruskalWallisRank samples = groupByTags
                          . sortBy (comparing fst)
                          . U.zip tags
                          $ rank (==) joinSample
  where
    (tags,joinSample) = U.unzip
                      . sortBy (comparing snd)
                      $ foldMap (uncurry tagSample) $ zip [(1::Int)..] samples
    tagSample = U.map . (,)

    groupByTags xs
        | U.null xs = []
        | otherwise = sort (U.map snd ys) : groupByTags zs
      where
        (ys,zs) = U.span ((==) (fst $ U.head xs) . fst) xs

kruskalWallis :: [Sample] -> Double
kruskalWallis samples = (n - 1) * numerator / denominator
  where
    n = sumWith rsamples $ fromIntegral . U.length
    avgRank = (n + 1) / 2
    numerator = sumWith rsamples $ \sample ->
        let avgRankSample = Sample.sum sample / size
            size = fromIntegral $ U.length sample
        in  size * square (avgRankSample - avgRank)
    denominator = sumWith rsamples $ \sample ->
        Sample.sum $ U.map (\r -> square (r - avgRank)) sample

    rsamples = kruskalWallisRank samples

sumWith :: [Sample] -> (Sample -> Double) -> Double
sumWith samples f = Prelude.sum $ fmap f samples

square :: Num a => a -> a
square x = x ^ (2::Int)
