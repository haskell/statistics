{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Statistics.Correlation.Spearman
--

module Statistics.Correlation.Spearman
    ( spearman
    , ranking
    ) where

import Statistics.Test.Internal (rank)
import Statistics.Correlation.Pearson (pearson)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Function (on)
import Data.Ord
import Statistics.Function

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
    (index, xs') = G.unzip . sortBy (comparing snd) . G.zip (G.fromList [0..n-1]) $ xs
    ranks = rank (==) xs'
    loop x v | x < n = do GM.unsafeWrite v (index G.! x) (ranks G.! x)
                          loop (x + 1) v
             | otherwise = return v
    n = G.length xs
{-# INLINE ranking #-}
