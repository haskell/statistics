{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.Exponential
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The exponential distribution.  This is the continunous probability
-- distribution of the times between events in a poisson process, in
-- which events occur continuously and independently at a constant
-- average rate.

module Statistics.Distribution.Exponential
    (
      ExponentialDistribution
    -- * Constructors
    , fromLambda
    , fromSample
    -- * Accessors
    , edLambda
    ) where

import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import qualified Statistics.Sample as S
import Statistics.Types (Sample)

newtype ExponentialDistribution = ED {
      edLambda :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution ExponentialDistribution where
    cumulative (ED l) x = 1 - exp (-l * x)
    {-# INLINE cumulative #-}

instance D.ContDistr ExponentialDistribution where
    density (ED l) x    = l * exp (-l * x)
    {-# INLINE density #-}
    quantile (ED l) p   = -log (1 - p) / l
    {-# INLINE quantile #-}

instance D.Variance ExponentialDistribution where
    variance (ED l) = 1 / (l * l)
    {-# INLINE variance #-}

instance D.Mean ExponentialDistribution where
    mean (ED l) = 1 / l
    {-# INLINE mean #-}

fromLambda :: Double            -- ^ &#955; (scale) parameter.
           -> ExponentialDistribution
fromLambda = ED
{-# INLINE fromLambda #-}

fromSample :: Sample -> ExponentialDistribution
fromSample = ED . S.mean
{-# INLINE fromSample #-}
