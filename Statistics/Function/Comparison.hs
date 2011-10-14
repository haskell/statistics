{-# LANGUAGE BangPatterns #-}
-- |
-- Module    : Statistics.Function.Comparison
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Approximate floating point comparison, based on Bruce Dawson's
-- \"Comparing floating point numbers\":
-- <http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm>

module Statistics.Function.Comparison
    (
      within
    ) where

import Debug.Trace
import Control.Monad.ST (runST)
import Data.Array.ST (newArray, castSTUArray, readArray, writeArray)
import Data.Int (Int64)

-- | Compare two 'Double' values for approximate equality, using
-- Dawson's method.
--
-- The required accuracy is specified in ULPs (units of least
-- precision).  If the two numbers differ by the given number of ULPs
-- or less, this function returns @True@.
within :: Int                   -- ^ Number of ULPs of accuracy desired.
       -> Double -> Double -> Bool
within ulps a b = runST go
  where go = do
          dary <- newArray (0,0::Int) a
          iary <- castSTUArray dary
          ai0 <- readArray iary 0
          writeArray dary 0 b
          bi0 <- readArray iary 0
          let ai | ai0 < 0   = big - ai0
                 | otherwise = ai0
              bi | bi0 < 0   = big - bi0
                 | otherwise = bi0
          return $ abs (ai - bi) <= fromIntegral ulps
        big = 0x8000000000000000 :: Int64
