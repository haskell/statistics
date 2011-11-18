{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module Tests.Transform
    (
      tests
    ) where

import Data.Bits             ((.&.), shiftL)
import Data.Complex          (Complex((:+)))
import Data.Functor          ((<$>))
import Statistics.Function   (within)
import Statistics.Transform

import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Positive(..),Property,Arbitrary(..),Gen,choose,vectorOf,
                                             printTestCase, quickCheck)

import Text.Printf

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Tests.Helpers



tests :: Test
tests = testGroup "fft" [
          testProperty "t_impulse"        t_impulse
        , testProperty "t_impulse_offset" t_impulse_offset
        , testProperty "ifft . fft = id"  (t_fftInverse $ ifft . fft)
        , testProperty "fft . ifft = id"  (t_fftInverse $ fft . ifft)
        , testProperty "idct . dct = id [up to scale]"
            (t_fftInverse (\v -> U.map (/ fromIntegral (U.length v)) $ idct $ dct v))
        , testProperty "dct . idct = id [up to scale]"
            (t_fftInverse (\v -> U.map (/ fromIntegral (U.length v)) $ idct $ dct v))
        ]

-- A single real-valued impulse at the beginning of an otherwise zero
-- vector should be replicated in every real component of the result,
-- and all the imaginary components should be zero.
t_impulse :: Double -> Positive Int -> Bool
t_impulse k (Positive m) = G.all (c_near i) (fft v)
  where v = i `G.cons` G.replicate (n-1) 0
        i = k :+ 0
        n = 1 `shiftL` (m .&. 6)

-- If a real-valued impulse is offset from the beginning of an
-- otherwise zero vector, the sum-of-squares of each component of the
-- result should equal the square of the impulse.
t_impulse_offset :: Double -> Positive Int -> Positive Int -> Bool
t_impulse_offset k (Positive x) (Positive m) = G.all ok (fft v)
  where v = G.concat [G.replicate xn 0, G.singleton i, G.replicate (n-xn-1) 0]
        ok (re :+ im) = within ulps (re*re + im*im) (k*k)
        i = k :+ 0
        xn = x `rem` n
        n = 1 `shiftL` (m .&. 6)

-- Test that (ifft . fft ≈ id)
--
-- Approximate equality here is tricky. Smaller values of vector tend
-- to have large relative error. Thus we should test that vectors as
-- whole are approximate equal.
t_fftInverse :: (HasNorm (U.Vector a), U.Unbox a, Num a, Show a, Arbitrary a)
             => (U.Vector a -> U.Vector a) -> Property
t_fftInverse roundtrip = do
  x <- genFftVector
  let n  = G.length x
      x' = roundtrip x
      d  = G.zipWith (-) x x'
      nd = vectorNorm d
      nx = vectorNorm x
  id $ printTestCase "Original vector"
     $ printTestCase (show x )
     $ printTestCase "Transformed one"
     $ printTestCase (show x')
     $ printTestCase (printf "Length = %i" n)
     $ printTestCase (printf "|x - x'| / |x| = %.6g" (nd / nx))
     $ nd <= 3e-14 * nx


----------------------------------------------------------------

-- With an error tolerance of 8 ULPs, a million QuickCheck tests are
-- likely to all succeed. With a tolerance of 7, we fail around the
-- half million mark.
ulps :: Int
ulps = 8

c_near :: CD -> CD -> Bool
c_near (a :+ b) (c :+ d) = within ulps a c && within ulps b d

genFftVector :: (U.Unbox a, Arbitrary a) => Gen (U.Vector a)
genFftVector = do
  n <- (2^)  <$> choose (1,9::Int)    -- Size of vector
  G.fromList <$> vectorOf n arbitrary -- Vector to transform

-- Ad-hoc type class for calculation of vector norm
class HasNorm a where
  vectorNorm :: a -> Double

instance HasNorm (U.Vector Double) where
  vectorNorm = sqrt . U.sum . U.map (\x -> x*x)

instance HasNorm (U.Vector CD) where
  vectorNorm = sqrt . U.sum . U.map (\(x :+ y) -> x*x + y*y)
