{-# LANGUAGE BangPatterns, FlexibleContexts #-}

-- |
-- Module      : Statistics.Correlation.Kendall
-- Description : Kendall's τ
--
-- Fast O(NlogN) implementation of Kendall's tau.

module Statistics.Correlation.Kendall 
    ( kendall ) where

import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Function
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.PrimRef

kendall :: (Ord a, G.Vector v (a, a)) => v (a, a) -> Double
{-# INLINABLE kendall #-}
kendall xy' = runST $ do
    xy <- G.thaw xy'
    let n = GM.length xy
        n_0 = (fromIntegral n * (fromIntegral n-1)) `shiftR` 1 :: Integer
    n_dis <- newPrimRef 0
    I.sort xy
    equalX <- numOfEqualBy ((==) `on` fst) xy
    tmp <- GM.new n
    mergeSort (compare `on` snd) xy tmp n_dis
    equalY <- numOfEqualBy ((==) `on` snd) xy
    n_d <- readPrimRef n_dis
    let nu = n_0 - n_d - equalX - equalY - n_d
        de = (n_0 - equalX) * (n_0 - equalY)
    return $ fromIntegral nu / (sqrt.fromIntegral) de

numOfEqualBy :: (PrimMonad m, GM.MVector v a)
             => (a -> a -> Bool) -> v (PrimState m) a -> m Integer
{-# INLINE numOfEqualBy #-}
numOfEqualBy f xs = do
    count <- newPrimRef (0::Integer)
    loop count (1::Int) (0::Int)
    readPrimRef count
    where
        n = GM.length xs
        loop c !acc !i | i >= n - 1 = modifyPrimRef' c (+ g acc)
                       | otherwise = do
                           x1 <- GM.unsafeRead xs i
                           x2 <- GM.unsafeRead xs (i+1)
                           if f x1 x2
                              then loop c (acc+1) (i+1)
                              else modifyPrimRef' c (+ g acc) >> loop c 1 (i+1)
        g x = fromIntegral ((x * (x - 1)) `shiftR` 1)

-- Adapted from vector-algorithm
mergeSort :: (PrimMonad m, GM.MVector v e)
          => (e -> e -> Ordering)
          -> v (PrimState m) e 
          -> v (PrimState m) e 
          -> PrimRef m Integer
          -> m ()
{-# INLINE mergeSort #-}
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
                           modifyPrimRef' count (+1) 
                  _ -> return ()
          | otherwise  = do
              let mid = (u + l) `shiftR` 1
              loop l mid
              loop mid u
              merge cmp (GM.unsafeSlice l (u-l+1) src) buf (mid - l) count

merge :: (PrimMonad m, GM.MVector v e)
      => (e -> e -> Ordering)
      -> v (PrimState m) e
      -> v (PrimState m) e
      -> Int
      -> PrimRef m Integer
      -> m ()
{-# INLINE merge #-}
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
                 modifyPrimRef' count (+ fromIntegral (GM.length low - iLow))
                 wroteHigh low iLow eLow high (iHigh+1) (iIns+1)
        _  -> do GM.unsafeWrite src iIns eLow
                 wroteLow low (iLow+1) high iHigh eHigh (iIns+1)

-- $references
--
-- * William R. Knight. (1966) A computer method for calculating Kendall's Tau
--   with ungrouped data. /Journal of the American Statistical Association/,
--   Vol. 61, No. 314, Part 1, pp. 436-439. (http://www.jstor.org/pss/2282833).
--
