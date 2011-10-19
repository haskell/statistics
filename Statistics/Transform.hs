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

import Debug.Trace
import Control.Arrow ((***))
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR)
import Data.Complex (Complex(..), realPart)
import Statistics.Math (log2)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U

type CD = Complex Double

-- | Discrete cosine transform.
dct :: V.Vector CD -> V.Vector Double
dct xs = G.map realPart $ G.zipWith (*) weights (fft interleaved)
  where
    interleaved = G.backpermute xs $ G.enumFromThenTo 0 2 (len-2) G.++
                                     G.enumFromThenTo (len-1) (len-3) 1
    weights = G.cons 1 . G.generate (len-1) $ \x ->
              2 * exp (negate i*fi (x+1)*pi/(2*n))
      where n = fi len
    len = G.length xs

-- | Inverse discrete cosine transform.
idct :: G.Vector v Double => V.Vector CD -> v Double
idct xs = G.generate len interleave
  where
    interleave z | even z    = vals `G.unsafeIndex` halve z
                 | otherwise = vals `G.unsafeIndex` (len - halve z - 1)
    vals = G.map realPart . ifft $ G.zipWith (*) weights xs
    weights = G.generate len $ \x -> n * exp (i*fi x*pi/(2*n))
      where n = fi len
    len = G.length xs

-- | Inverse fast Fourier transform.
ifft :: V.Vector CD -> V.Vector CD
ifft xs = G.map (/fi (G.length xs)) (fft xs)

fft v = let x = fft2 v
        in traceShow (v,x) x

fft1 :: V.Vector CD -> V.Vector CD
fft1 xs = case G.length xs of
            0 -> G.empty
            2 -> dft xs
            _ -> G.zipWith (flip ($)) (zs G.++ zs) .
                 G.zipWith (flip ($)) (ys G.++ ys) $
                 (G.map f ks G.++ G.map g ks) 
  where (ys,zs) = (fft1***fft1) . G.unzip . pairs $ xs
        ks = G.enumFromTo 0 ((len*2)-1)
        f k x y = x + y * exp (negate(2*pi*i*fi k)/fi(len*2))
        g k x y = x - y * exp (negate(2*pi*i*fi k)/fi(len*2))
        pairs v = G.generate (halve (G.length v)) $ \j ->
                  let z = j * 2
                  in (v `G.unsafeIndex` z, v `G.unsafeIndex` (z+1))
        len = G.length ys

-- | Fast Fourier transform.
fft2 :: V.Vector CD -> V.Vector CD
fft2 v = G.create $ do
           mv <- G.thaw v
           mfft mv
           return mv

mfft :: (M.MVector v CD) => v s CD -> ST s ()
mfft vec = do
  bitReverse 0 0
  stage 0 1 (-1) 0
 where
  bitReverse i j | i == (len-1) = return ()
                 | otherwise = do
    when (i < j) $ M.swap vec i j
    let inner k l | k <= l    = inner (l-k) (k `shiftR` 1)
                  | otherwise = bitReverse (i+1) (l+k)
    inner (len `shiftR` 1) j
  stage l !l1 !c1 !c2 | l == log2 len = return ()
                      | otherwise = do
    let !l2 = l1 `shiftL` 1
        flight j !u1 !u2 | j == l1 = return ()
                         | otherwise = do
          let butterfly i | i >= len = return ()
                          | otherwise = do
                let i1 = i + l1
                ci1@(xi1 :+ yi1) <- M.read vec i1
                let d = (u1*xi1 - u2*yi1) :+ (u1*yi1 + u2*xi1)
                ci <- M.read vec i
                M.write vec i1 (ci - d)
                M.write vec i (ci + d)
                butterfly (i+l2)
          butterfly j
          flight (j+1) (u1*c1 - u2*c2) (u1*c2 + u2*c1)
    flight 0 1 0
    stage (l+1) l2 (sqrt ((1+c1) / 2)) (sqrt ((1-c1) / 2))
  len = M.length vec

-- | Discrete Fourier transform.
dft :: V.Vector CD -> V.Vector CD
dft xs = G.generate len go
  where go k = G.sum . flip G.imap xs $ \n j ->
               j * exp (negate(2*pi*i*fi n*fi k)/fi len)
        len = G.length xs

fi :: Int -> CD
fi = fromIntegral

i :: CD
i = 0 :+ 1

halve :: Int -> Int
halve = (`shiftR` 1)
