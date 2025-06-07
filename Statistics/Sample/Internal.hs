{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Statistics.Sample.Internal
-- Copyright : (c) 2013 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Internal functions for computing over samples.
module Statistics.Sample.Internal
    (
      robustSumVar
    , sum
    , sumF
    ) where

import qualified Numeric.Sum as Sum
import Prelude hiding (sum)
import Statistics.Function (square)
import qualified Data.Vector.Generic as G

robustSumVar :: (G.Vector v Double) => Double -> v Double -> Double
robustSumVar m = sum . G.map (square . subtract m)
{-# INLINE robustSumVar #-}

sum :: (G.Vector v Double) => v Double -> Double
sum = Sum.sumVector Sum.kbn
{-# INLINE sum #-}

sumF :: Foldable f => f Double -> Double
sumF = Sum.sum Sum.kbn
{-# INLINE sumF #-}
