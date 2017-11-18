{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Statistics.Sample.Normalize
-- Copyright : (c) 2017 Gregory W. Schwartz
-- License   : BSD3
--
-- Maintainer  : gsch@mail.med.upenn.edu
-- Stability   : experimental
-- Portability : portable
--
-- Functions for normalizing samples.

module Statistics.Sample.Normalize
    (
      standardize
    ) where

import Statistics.Sample
import qualified Data.Vector.Generic as G

-- | /O(n)/ Normalize a sample using standard scores.
standardize :: (G.Vector v Double) => v Double -> v Double
standardize xs = G.map (\x -> (x - mu) / sigma) xs
  where
    mu    = mean xs
    sigma = stdDev xs
