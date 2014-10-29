{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Statistics.Correlation.Spearman
--

module Statistics.Correlation.Spearman
    ( spearman
    , spearmanMatByRow
    , ranking
    ) where

import Data.Function (on)
import Data.Ord
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Statistics.Correlation.Pearson
import Statistics.Function
import Statistics.Matrix hiding (map)
import Statistics.Test.Internal (rank)

-- | compute spearman correlation between two samples
spearman :: ( Ord a
            , G.Vector v a
            , G.Vector v Double
            , G.Vector v Int
            , G.Vector v (Int, a)
            )
         => v a
         -> v a
         -> Double
spearman x y | G.length x /= G.length y = error "Statistics.Correlation.Spearman.spearman: Incompatible dimensions"
             | otherwise = (pearson `on` ranking) x y
{-# INLINE spearman #-}

-- | compute pairwise spearman correlation between rows of a matrix
spearmanMatByRow :: Matrix -> Matrix
spearmanMatByRow = pearsonMatByRow . fromRows . map ranking . toRows
{-# INLINE spearmanMatByRow #-}

-- calculate rank of sample. Sample could be unsorted
ranking :: ( Ord a
           , G.Vector v a
           , G.Vector v Double
           , G.Vector v Int
           , G.Vector v (Int, a)
           )
        => v a
        -> v Double
ranking xs = G.create $ GM.replicate n 0 >>= loop 0
  where
    (index, xs') = G.unzip . sortBy (comparing snd) . G.zip (G.enumFromN 0 n) $ xs
    ranks = rank (==) xs'
    loop x v | x < n = do GM.unsafeWrite v (index G.! x) (ranks G.! x)
                          loop (x + 1) v
             | otherwise = return v
    n = G.length xs
{-# INLINE ranking #-}
