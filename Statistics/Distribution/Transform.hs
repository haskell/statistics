{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts,
    FlexibleInstances, UndecidableInstances #-}
-- |
-- Module    : Statistics.Distribution.Transform
-- Copyright : (c) 2013 John McDonnell;
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Transformations over distributions
module Statistics.Distribution.Transform (
    LinearTransform (..)
  , linTransFixedPoint
  , scaleAround
  ) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Functor          ((<$>))
import qualified Statistics.Distribution as D

-- | Linear transformation applied to distribution.
--
-- > LinearTransform μ σ _
-- > x' = μ + σ·x
data LinearTransform d = LinearTransform
  { linTransLocation :: {-# UNPACK #-} !Double
    -- ^ Location parameter.
  , linTransScale    :: {-# UNPACK #-} !Double
    -- ^ Scale parameter.
  , linTransDistr    :: d
    -- ^ Distribution being transformed.
  } deriving (Eq, Show, Read, Typeable, Data, Generic)

instance (Binary d) => Binary (LinearTransform d)

-- | Apply linear transformation to distribution.
scaleAround :: Double           -- ^ Fixed point
            -> Double           -- ^ Scale parameter
            -> d                -- ^ Distribution
            -> LinearTransform d
scaleAround x0 sc = LinearTransform (x0 * (1 - sc)) sc

-- | Get fixed point of linear transformation
linTransFixedPoint :: LinearTransform d -> Double
linTransFixedPoint (LinearTransform loc sc _) = loc / (1 - sc)

instance Functor LinearTransform where
  fmap f (LinearTransform loc sc dist) = LinearTransform loc sc (f dist)

instance D.Distribution d => D.Distribution (LinearTransform d) where
  cumulative (LinearTransform loc sc dist) x = D.cumulative dist $ (x-loc) / sc

instance D.ContDistr d => D.ContDistr (LinearTransform d) where
  density  (LinearTransform loc sc dist) x = D.density dist ((x-loc) / sc) / sc
  quantile (LinearTransform loc sc dist) p = loc + sc * D.quantile dist p

instance D.MaybeMean d => D.MaybeMean (LinearTransform d) where
  maybeMean (LinearTransform loc _ dist) = (+loc) <$> D.maybeMean dist

instance (D.Mean d) => D.Mean (LinearTransform d) where
  mean (LinearTransform loc _ dist) = loc + D.mean dist

instance D.MaybeVariance  d => D.MaybeVariance (LinearTransform d) where
  maybeVariance (LinearTransform _ sc dist) = (*(sc*sc)) <$> D.maybeVariance dist
  maybeStdDev   (LinearTransform _ sc dist) = (*sc)      <$> D.maybeStdDev dist

instance (D.Variance d) => D.Variance (LinearTransform d) where
  variance (LinearTransform _ sc dist) = sc * sc * D.variance dist
  stdDev   (LinearTransform _ sc dist) = sc * D.stdDev dist

instance D.ContGen d => D.ContGen (LinearTransform d) where
  genContVar (LinearTransform loc sc d) g = do
    x <- D.genContVar d g
    return $! loc + sc * x
