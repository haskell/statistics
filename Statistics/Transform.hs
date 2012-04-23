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
-- Fourier-related transformations of mathematical functions.
--
-- These functions are written for simplicity and correctness, not
-- speed.  If you need a fast FFT implementation for your application,
-- you should strongly consider using a library of FFTW bindings
-- instead.

module Statistics.Transform
    (
    -- * Type synonyms
      CD
    -- * Discrete cosine transform
    , dct
    , dct_
    , idct
    , idct_
    -- * Fast Fourier transform
    , fft
    , ifft
    ) where

import Control.Monad         (when)
import Control.Monad.ST      (ST)
import Data.Bits             (shiftL, shiftR)
import Data.Complex          (Complex(..), conjugate, realPart)
import Numeric.SpecFunctions (log2)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U


type CD = Complex Double

-- | Discrete cosine transform (DCT-II).
dct :: U.Vector Double -> U.Vector Double
dct = dct_ . G.map (:+0)

-- | Discrete cosine transform, with complex coefficients (DCT-II).
dct_ :: U.Vector CD -> U.Vector Double
dct_ xs = G.map realPart $ G.zipWith (*) weights (fft interleaved)
  where
    interleaved = G.backpermute xs $ G.enumFromThenTo 0 2 (len-2) G.++
                                     G.enumFromThenTo (len-1) (len-3) 1
    weights = G.cons 2 . G.generate (len-1) $ \x ->
              2 * exp ((0:+(-1))*fi (x+1)*pi/(2*n))
      where n = fi len
    len = G.length xs

-- | Inverse discrete cosine transform (DCT-III). It's inverse of
-- 'dct' only up to scale parameter:
--
-- > (idct . dct) x = (* lenngth x)
idct :: U.Vector Double -> U.Vector Double
idct = idct_ . G.map (:+0)

-- | Inverse discrete cosine transform, with complex coefficients
-- (DCT-III).
idct_ :: U.Vector CD -> U.Vector Double
idct_ xs = G.generate len interleave
  where
    interleave z | even z    = vals `G.unsafeIndex` halve z
                 | otherwise = vals `G.unsafeIndex` (len - halve z - 1)
    vals = G.map realPart . ifft $ G.zipWith (*) weights xs
    weights 
      = G.cons n
      $ G.generate (len - 1) $ \x -> 2 * n * exp ((0:+1) * fi (x+1) * pi/(2*n))
      where n = fi len
    len = G.length xs

-- | Inverse fast Fourier transform.
ifft :: U.Vector CD -> U.Vector CD
ifft xs = G.map ((/fi (G.length xs)) . conjugate) . fft . G.map conjugate $ xs

-- | Radix-2 decimation-in-time fast Fourier transform.
fft :: U.Vector CD -> U.Vector CD
fft v = G.create $ do
          mv <- G.thaw v
          mfft mv
          return mv

mfft :: (M.MVector v CD) => v s CD -> ST s ()
mfft vec
    | 1 `shiftL` m /= len = error "Statistics.Transform.fft: bad vector size"
    | otherwise           = bitReverse 0 0
 where
  bitReverse i j | i == len-1 = stage 0 1
                 | otherwise  = do
    when (i < j) $ M.swap vec i j
    let inner k l | k <= l    = inner (k `shiftR` 1) (l-k)
                  | otherwise = bitReverse (i+1) (l+k)
    inner (len `shiftR` 1) j
  stage l !l1 | l == m    = return ()
              | otherwise = do
    let !l2 = l1 `shiftL` 1
        !e  = -6.283185307179586/fromIntegral l2
        flight j !a | j == l1   = stage (l+1) l2
                    | otherwise = do
          let butterfly i | i >= len  = flight (j+1) (a+e)
                          | otherwise = do
                let i1 = i + l1
                xi1 :+ yi1 <- M.read vec i1
                let !c = cos a
                    !s = sin a
                    d  = (c*xi1 - s*yi1) :+ (s*xi1 + c*yi1)
                ci <- M.read vec i
                M.write vec i1 (ci - d)
                M.write vec i (ci + d)
                butterfly (i+l2)
          butterfly j
    flight 0 0
  len = M.length vec
  m   = log2 len

fi :: Int -> CD
fi = fromIntegral

halve :: Int -> Int
halve = (`shiftR` 1)
