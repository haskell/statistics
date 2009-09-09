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
-- The exponential distribution.  This is the discrete probability
-- distribution of the number of successes in a sequence of /n/
-- independent yes\/no experiments, each of which yields success with
-- probability /p/.

module Statistics.Distribution.Exponential
    (
      ExponentialDistribution
    -- * Constructors
    , fromLambda
    , fromSample
    ) where

import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import qualified Statistics.Sample as S
import Statistics.Types (Sample)

newtype ExponentialDistribution = ED {
      edLambda :: Double
    } deriving (Eq, Read, Show, Typeable)

instance D.Distribution ExponentialDistribution where
    probability (ED l) x = l * exp (-l * x)
    {-# INLINE probability #-}
    cumulative (ED l) x  = 1 - exp (-l * x)
    {-# INLINE cumulative #-}
    inverse (ED l) p     = -log (1 - p) / l
    {-# INLINE inverse #-}

instance D.Variance ExponentialDistribution where
    variance (ED l) = l * l
    {-# INLINE variance #-}

instance D.Mean ExponentialDistribution where
    mean = edLambda
    {-# INLINE mean #-}

fromLambda :: Double            -- ^ &#955; (scale) parameter.
           -> ExponentialDistribution
fromLambda = ED
{-# INLINE fromLambda #-}

fromSample :: Sample -> ExponentialDistribution
fromSample = ED . S.mean
{-# INLINE fromSample #-}
