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
import qualified Data.Vector.Generic  as G
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S

-- | /O(n)/ Normalize a sample using standard scores:
--
--   \[ z = \frac{x - \mu}{\sigma} \]
--
--   Where μ is sample mean and σ is standard deviation computed from
--   unbiased variance estimation. If sample to small to compute σ or
--   it's equal to 0 @Nothing@ is returned.
standardize :: (G.Vector v Double) => v Double -> Maybe (v Double)
standardize xs
  | G.length xs < 2 = Nothing
  | sigma == 0      = Nothing
  | otherwise       = Just $ G.map (\x -> (x - mu) / sigma) xs
  where
    mu    = mean   xs
    sigma = stdDev xs
{-# INLINABLE  standardize #-}
{-# SPECIALIZE standardize :: V.Vector Double -> Maybe (V.Vector Double) #-}
{-# SPECIALIZE standardize :: U.Vector Double -> Maybe (U.Vector Double) #-}
{-# SPECIALIZE standardize :: S.Vector Double -> Maybe (S.Vector Double) #-}
