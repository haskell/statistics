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

import Control.Arrow ((***))
import Data.Bits (shiftR)
import Data.Complex (Complex(..), realPart)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

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

-- | Fast Fourier transform.
fft :: V.Vector CD -> V.Vector CD
fft xs = case G.length xs of
            0 -> G.empty
            2 -> dft xs
            _ -> G.zipWith (flip ($)) (zs G.++ zs) .
                 G.zipWith (flip ($)) (ys G.++ ys) $
                 (G.map f ks G.++ G.map g ks) 
  where (ys,zs) = (fft***fft) . G.unzip . pairs $ xs
        ks = G.enumFromTo 0 ((len*2)-1)
        f k x y = x + y * exp (negate(2*pi*i*fi k)/fi(len*2))
        g k x y = x - y * exp (negate(2*pi*i*fi k)/fi(len*2))
        pairs v = G.generate (halve (G.length v)) $ \j ->
                  let z = j * 2
                  in (v `G.unsafeIndex` z, v `G.unsafeIndex` (z+1))
        len = G.length ys

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
