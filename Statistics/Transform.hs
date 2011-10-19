{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- |
-- Module    : Statistics.Transform
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Transformations of functions.
--
-- These functions are not particularly fast, so they're not yet
-- public.

module Statistics.Transform
    (
      CD
    , dct
    , idct
    , fft
    , ifft
    ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR)
import Data.Complex (Complex(..), conjugate, realPart)
import Statistics.Math (log2)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U

type CD = Complex Double

-- | Discrete cosine transform.
dct :: U.Vector CD -> U.Vector Double
dct xs = G.map realPart $ G.zipWith (*) weights (fft interleaved)
  where
    interleaved = G.backpermute xs $ G.enumFromThenTo 0 2 (len-2) G.++
                                     G.enumFromThenTo (len-1) (len-3) 1
    weights = G.cons 1 . G.generate (len-1) $ \x ->
              2 * exp ((0:+(-1))*fi (x+1)*pi/(2*n))
      where n = fi len
    len = G.length xs

-- | Inverse discrete cosine transform.
idct :: U.Vector CD -> U.Vector Double
idct xs = G.generate len interleave
  where
    interleave z | even z    = vals `G.unsafeIndex` halve z
                 | otherwise = vals `G.unsafeIndex` (len - halve z - 1)
    vals = G.map realPart . ifft $ G.zipWith (*) weights xs
    weights = G.generate len $ \x -> n * exp ((0:+1)*fi x*pi/(2*n))
      where n = fi len
    len = G.length xs

-- | Inverse fast Fourier transform.
ifft :: U.Vector CD -> U.Vector CD
ifft xs = G.map ((/fi (G.length xs)) . conjugate) . fft . G.map conjugate $ xs

-- | Fast Fourier transform.
fft :: U.Vector CD -> U.Vector CD
fft v = G.create $ do
          mv <- G.thaw v
          mfft mv
          return mv

mfft :: (M.MVector v CD) => v s CD -> ST s ()
mfft vec = do
  bitReverse 0 0
  stage 0 1
 where
  bitReverse i j | i == (len-1) = return ()
                 | otherwise = do
    when (i < j) $ M.swap vec i j
    let inner k l | k <= l    = inner (l-k) (k `shiftR` 1)
                  | otherwise = bitReverse (i+1) (l+k)
    inner (len `shiftR` 1) j
  stage l !l1 | l == log2 len = return ()
              | otherwise = do
    let !l2 = l1 `shiftL` 1
        !e  = -6.283185307179586/fromIntegral l2
        flight j !a | j == l1 = return ()
                    | otherwise = do
          let butterfly i | i >= len = return ()
                          | otherwise = do
                let !c = cos a
                    !s = sin a
                let i1 = i + l1
                xi1 :+ yi1 <- M.read vec i1
                let d = (c*xi1 - s*yi1) :+ (s*xi1 + c*yi1)
                ci <- M.read vec i
                M.write vec i1 (ci - d)
                M.write vec i (ci + d)
                butterfly (i+l2)
          butterfly j
          flight (j+1) (a+e)
    flight 0 0
    stage (l+1) l2
  len = M.length vec

fi :: Int -> CD
fi = fromIntegral

halve :: Int -> Int
halve = (`shiftR` 1)
