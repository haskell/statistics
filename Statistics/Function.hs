-- |
-- Module    : Statistics.Quantile
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for computing quantiles.

module Statistics.Function
    (
      sort
    ) where

import Data.Array.Vector (UA, UArr)
import Data.Array.Vector.Algorithms.Immutable (apply)
import qualified Data.Array.Vector.Algorithms.Intro as I (sort)
import Statistics.Types (Sorted(..))

sort :: (UA a, Ord a) => UArr a -> Sorted (UArr a)
sort a = Sorted (apply I.sort a)
