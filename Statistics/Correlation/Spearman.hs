{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Statistics.Correlation.Spearman
--

module Statistics.Correlation.Spearman
    ( spearman
    , spearmanMatByRow
    , ranking
    ) where

import Data.Ord
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Statistics.Correlation.Pearson
import Statistics.Function
import Statistics.Matrix hiding (map)
import Statistics.Test.Internal (rank)

-- | compute spearman correlation between two samples
spearman :: ( Ord a
            , Ord b
            , G.Vector v a
            , G.Vector v b
            , G.Vector v (a, b)
            , G.Vector v Int
            , G.Vector v Double
            , G.Vector v (Double, Double)
            , G.Vector v (Int, Double)
            , G.Vector v (Int, a)
            , G.Vector v (Int, b)
            )
         => v (a, b)
         -> Double
spearman xy = pearson xy'
  where
    xy' = G.zip (ranking x) $ ranking y    
    (x, y) = G.unzip xy
{-# INLINE spearman #-}

-- | compute pairwise spearman correlation between rows of a matrix
spearmanMatByRow :: Matrix -> Matrix
spearmanMatByRow = pearsonMatByRow . fromRows . map ranking . toRows
{-# INLINE spearmanMatByRow #-}

-- Note: for sorted data, use Statistics.Test.Internal.rank
-- | compute ranks for unsorted data
ranking :: ( Ord a
           , G.Vector v a
           , G.Vector v Int
           , G.Vector v Double
           , G.Vector v (Int, a)
           , G.Vector v (Int, Double)
           )
        => v a
        -> v Double
ranking xs = G.create $ do
    vec <- GM.new n
    G.mapM_ (f vec) . G.zip index $ ranks
    return vec
  where
    (index, xs') = G.unzip . sortBy (comparing snd) . G.zip (G.enumFromN 0 n) $ xs
    ranks = rank (==) xs'
    f v (idx, val) = GM.unsafeWrite v idx val
    n = G.length xs
{-# INLINE ranking #-}
