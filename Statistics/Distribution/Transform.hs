{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveDataTypeable #-}
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
  ) where

import qualified Statistics.Distribution as D
import Data.Typeable         (Typeable)

data LinearTransform d = LinearTransform
  { location :: {-# UNPACK #-} !Double
  , scale :: {-# UNPACK #-} !Double
  , distr :: d
  } deriving (Eq,Show,Read,Typeable)

instance Functor LinearTransform where
  fmap f (LinearTransform loc sc dist) = LinearTransform loc sc (f dist)

instance D.Distribution d => D.Distribution (LinearTransform d) where
  cumulative (LinearTransform loc sc dist) x = D.cumulative dist ((x-loc)/sc)

instance D.ContDistr d => D.ContDistr (LinearTransform d) where
  density (LinearTransform loc sc dist) x = (/sc) $ D.density dist ((x-loc)/sc)
  quantile (LinearTransform loc sc dist) p = loc + sc * D.quantile dist p 

instance D.MaybeMean d => D.MaybeMean (LinearTransform d) where
  maybeMean (LinearTransform loc _ dist) = fmap (+loc) (D.maybeMean dist)

instance (D.Mean d) => D.Mean (LinearTransform d) where
  mean (LinearTransform loc _ dist) = loc + (D.mean dist)

instance D.MaybeVariance  d => D.MaybeVariance (LinearTransform d) where
  maybeVariance (LinearTransform _ sc dist) = fmap ((*sc).(*sc)) (D.maybeVariance dist)
  maybeStdDev (LinearTransform _ sc dist) = fmap (*sc) (D.maybeStdDev dist)

instance (D.Variance d) => D.Variance (LinearTransform d) where
  variance (LinearTransform _ sc dist) = sc * sc * (D.variance dist)
  stdDev (LinearTransform _ sc dist) = sc * (D.stdDev dist)

instance D.ContDistr d => D.ContGen (LinearTransform d) where
  genContVar = D.genContinous

