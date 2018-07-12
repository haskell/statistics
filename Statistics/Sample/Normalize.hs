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

import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(..))
import Statistics.Sample
import Statistics.Types    (StatisticsException(..))
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
standardize :: (G.Vector v Double, MonadThrow m) => v Double -> m (v Double)
standardize xs 
  | G.length xs < 2 = modErr "standardize" "Insufficient sample size"
  | otherwise       = do mu    <- mean   xs
                         sigma <- stdDev xs
                         when (sigma == 0) $
                           modErr "standardize" "Sample have zero variance"
                         return $! G.map (\x -> (x - mu) / sigma) xs
{-# SPECIALIZE standardize :: MonadThrow m => V.Vector Double -> m (V.Vector Double) #-}
{-# SPECIALIZE standardize :: MonadThrow m => U.Vector Double -> m (U.Vector Double) #-}
{-# SPECIALIZE standardize :: MonadThrow m => S.Vector Double -> m (S.Vector Double) #-}


modErr :: MonadThrow m => String -> String -> m a
modErr f err = throwM $ InvalidSample ("Statistics.Sample.Normalize" ++ f) err
