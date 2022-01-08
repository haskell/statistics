{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- |
-- Module      : Statistics.Correlation.Kendall
--
-- Fast O(NlogN) implementation of
-- <http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient Kendall's tau>.
--
-- This module implements Kendall's tau form b which allows ties in the data.
-- This is the same formula used by other statistical packages, e.g., R, matlab.
--
-- > \tau = \frac{n_c - n_d}{\sqrt{(n_0 - n_1)(n_0 - n_2)}}
--
-- where n_0 = n(n-1)\/2, n_1 = number of pairs tied for the first quantify,
-- n_2 = number of pairs tied for the second quantify,
-- n_c = number of concordant pairs$, n_d = number of discordant pairs.

module Statistics.Correlation.Kendall
    ( kendall

    -- * References
    -- $references
    ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR)
import Data.Function (on)
import Data.STRef
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

-- | /O(nlogn)/ Compute the Kendall's tau from a vector of paired data.
-- Return NaN when number of pairs <= 1.
kendall :: (Ord a, Ord b, G.Vector v (a, b)) => v (a, b) -> Double
kendall xy'
  | G.length xy' <= 1 = 0/0
  | otherwise  = runST $ do
    xy <- G.thaw xy'
    let n = GM.length xy
    n_dRef <- newSTRef 0
    I.sort xy
    tieX <- numOfTiesBy ((==) `on` fst) xy
    tieXY <- numOfTiesBy (==) xy
    tmp <- GM.new n
    mergeSort (compare `on` snd) xy tmp n_dRef
    tieY <- numOfTiesBy ((==) `on` snd) xy
    n_d <- readSTRef n_dRef
    let n_0 = (fromIntegral n * (fromIntegral n-1)) `shiftR` 1 :: Integer
        n_c = n_0 - n_d - tieX - tieY + tieXY
    return $ fromIntegral (n_c - n_d) /
             (sqrt.fromIntegral) ((n_0 - tieX) * (n_0 - tieY))
{-# INLINE kendall #-}

-- calculate number of tied pairs in a sorted vector
numOfTiesBy :: GM.MVector v a
            => (a -> a -> Bool) -> v s a -> ST s Integer
numOfTiesBy f xs = do count <- newSTRef (0::Integer)
                      loop count (1::Int) (0::Int)
                      readSTRef count
  where
    n = GM.length xs
    loop c !acc !i | i >= n - 1 = modifySTRef' c (+ g acc)
                   | otherwise = do
                       x1 <- GM.unsafeRead xs i
                       x2 <- GM.unsafeRead xs (i+1)
                       if f x1 x2
                          then loop c (acc+1) (i+1)
                          else modifySTRef' c (+ g acc) >> loop c 1 (i+1)
    g x = fromIntegral ((x * (x - 1)) `shiftR` 1)
{-# INLINE numOfTiesBy #-}

-- Implementation of Knight's merge sort (adapted from vector-algorithm). This
-- function is used to count the number of discordant pairs.
mergeSort :: GM.MVector v e
          => (e -> e -> Ordering)
          -> v s e
          -> v s e
          -> STRef s Integer
          -> ST s ()
mergeSort cmp src buf count = loop 0 (GM.length src - 1)
  where
    loop l u
      | u == l = return ()
      | u - l == 1 = do
          eL <- GM.unsafeRead src l
          eU <- GM.unsafeRead src u
          case cmp eL eU of
              GT -> do GM.unsafeWrite src l eU
                       GM.unsafeWrite src u eL
                       modifySTRef' count (+1)
              _ -> return ()
      | otherwise  = do
          let mid = (u + l) `shiftR` 1
          loop l mid
          loop mid u
          merge cmp (GM.unsafeSlice l (u-l+1) src) buf (mid - l) count
{-# INLINE mergeSort #-}

merge :: GM.MVector v e
      => (e -> e -> Ordering)
      -> v s e
      -> v s e
      -> Int
      -> STRef s Integer
      -> ST s ()
merge cmp src buf mid count = do GM.unsafeCopy tmp lower
                                 eTmp <- GM.unsafeRead tmp 0
                                 eUpp <- GM.unsafeRead upper 0
                                 loop tmp 0 eTmp upper 0 eUpp 0
  where
    lower = GM.unsafeSlice 0 mid src
    upper = GM.unsafeSlice mid (GM.length src - mid) src
    tmp = GM.unsafeSlice 0 mid buf
    wroteHigh low iLow eLow high iHigh iIns
      | iHigh >= GM.length high =
          GM.unsafeCopy (GM.unsafeSlice iIns (GM.length low - iLow) src)
                        (GM.unsafeSlice iLow (GM.length low - iLow) low)
      | otherwise = do eHigh <- GM.unsafeRead high iHigh
                       loop low iLow eLow high iHigh eHigh iIns

    wroteLow low iLow high iHigh eHigh iIns
      | iLow  >= GM.length low  = return ()
      | otherwise = do eLow <- GM.unsafeRead low iLow
                       loop low iLow eLow high iHigh eHigh iIns

    loop !low !iLow !eLow !high !iHigh !eHigh !iIns = case cmp eHigh eLow of
        LT -> do GM.unsafeWrite src iIns eHigh
                 modifySTRef' count (+ fromIntegral (GM.length low - iLow))
                 wroteHigh low iLow eLow high (iHigh+1) (iIns+1)
        _  -> do GM.unsafeWrite src iIns eLow
                 wroteLow low (iLow+1) high iHigh eHigh (iIns+1)
{-# INLINE merge #-}

-- $references
--
-- * William R. Knight. (1966) A computer method for calculating Kendall's Tau
--   with ungrouped data. /Journal of the American Statistical Association/,
--   Vol. 61, No. 314, Part 1, pp. 436-439. <http://www.jstor.org/pss/2282833>
--
