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
    ) where

import Numeric.Sum (kbn, sumVector)
import Prelude hiding (sum)
import Statistics.Function (square)
import qualified Data.Vector.Generic as G

robustSumVar :: (G.Vector v Double) => Double -> v Double -> Double
robustSumVar m = sum . G.map (square . subtract m)
{-# INLINE robustSumVar #-}

sum :: (G.Vector v Double) => v Double -> Double
sum = sumVector kbn
{-# INLINE sum #-}
