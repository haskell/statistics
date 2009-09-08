-- |
-- Module    : Statistics.Types
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Types for working with statistics.

module Statistics.Types
    (
      Sample
    , Estimator
    , Weights
    ) where

import Data.Array.Vector (UArr)

type Sample = UArr Double

-- | A function that estimates a property of a sample, such as 'mean'.
type Estimator = Sample -> Double

type Weights = UArr Double
