{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
-- |
-- Module    : Statistics.Distribution.DiscreteUniform
-- Copyright : (c) 2016 André Szabolcs Szelp
-- License   : BSD3
--
-- Maintainer  : a.sz.szelp@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The discrete uniform distribution. There are two parametrizations of
-- this distribution. First is the probability distribution on an
-- inclusive interval {1, ..., n}. This is parametrized with n only,
-- where p_1, ..., p_n = 1/n. ('discreteUniform').
--
-- The second parametrization is the uniform distribution on {a, ..., b} with
-- probabilities p_a, ..., p_b = 1/(a-b+1). This is parametrized with
-- /a/ and /b/. ('discreteUniformAB')

module Statistics.Distribution.DiscreteUniform
    (
      DiscreteUniform
    -- * Constructors
    , discreteUniform
    , discreteUniformAB
    -- * Accessors
    , rangeFrom
    , rangeTo
    ) where

import Control.Applicative (empty)
import Data.Aeson   (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary  (Binary(..))
import Data.Data    (Data, Typeable)
import System.Random.Stateful (uniformRM)
import GHC.Generics (Generic)

import qualified Statistics.Distribution as D
import Statistics.Internal



-- | The discrete uniform distribution.
data DiscreteUniform = U {
      rangeFrom  :: {-# UNPACK #-} !Int
    -- ^ /a/, the lower bound of the support {a, ..., b}
    , rangeTo    :: {-# UNPACK #-} !Int
    -- ^ /b/, the upper bound of the support {a, ..., b}
    } deriving (Eq, Typeable, Data, Generic)

instance Show DiscreteUniform where
  showsPrec i (U a b) = defaultShow2 "discreteUniformAB" a b i
instance Read DiscreteUniform where
  readPrec = defaultReadPrecM2 "discreteUniformAB" (\a b -> Just (discreteUniformAB a b))

instance ToJSON   DiscreteUniform
instance FromJSON DiscreteUniform where
  parseJSON (Object v) = do
    a <- v .: "uniformA"
    b <- v .: "uniformB"
    return $ discreteUniformAB a b
  parseJSON _ = empty

instance Binary DiscreteUniform where
  put (U a b) = put a >> put b
  get         = discreteUniformAB <$> get <*> get

instance D.Distribution DiscreteUniform where
  cumulative (U a b) x
    | x < fromIntegral a = 0
    | x > fromIntegral b = 1
    | otherwise = fromIntegral (floor x - a + 1) / fromIntegral (b - a + 1)

instance D.DiscreteDistr DiscreteUniform where
  probability (U a b) k
    | k >= a && k <= b = 1 / fromIntegral (b - a + 1)
    | otherwise        = 0

instance D.Mean DiscreteUniform where
  mean (U a b) = fromIntegral (a+b)/2

instance D.Variance DiscreteUniform where
  variance (U a b) = (fromIntegral (b - a + 1)^(2::Int) - 1) / 12

instance D.MaybeMean DiscreteUniform where
  maybeMean = Just . D.mean

instance D.MaybeVariance DiscreteUniform where
  maybeStdDev   = Just . D.stdDev
  maybeVariance = Just . D.variance

instance D.Entropy DiscreteUniform where
  entropy (U a b) = log $ fromIntegral $ b - a + 1

instance D.MaybeEntropy DiscreteUniform where
  maybeEntropy = Just . D.entropy

instance D.ContGen DiscreteUniform where
  genContVar d = fmap fromIntegral . D.genDiscreteVar d

instance D.DiscreteGen DiscreteUniform where
  genDiscreteVar (U a b) = uniformRM (a,b)

-- | Construct discrete uniform distribution on support {1, ..., n}.
--   Range /n/ must be >0.
discreteUniform :: Int             -- ^ Range
                -> DiscreteUniform
discreteUniform n
  | n < 1     = error $ msg ++ "range must be > 0. Got " ++ show n
  | otherwise = U 1 n
  where msg = "Statistics.Distribution.DiscreteUniform.discreteUniform: "

-- | Construct discrete uniform distribution on support {a, ..., b}.
discreteUniformAB :: Int             -- ^ Lower boundary (inclusive)
                  -> Int             -- ^ Upper boundary (inclusive)
                  -> DiscreteUniform
discreteUniformAB a b
  | b < a     = U b a
  | otherwise = U a b
