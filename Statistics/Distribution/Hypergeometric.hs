{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Hypergeometric
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The Hypergeometric distribution.  This is the discrete probability
-- distribution that measures the probability of /k/ successes in /l/
-- trials, without replacement, from a finite population.
--
-- The parameters of the distribution describe /k/ elements chosen
-- from a population of /l/, with /m/ elements of one type, and
-- /l/-/m/ of the other (all are positive integers).

module Statistics.Distribution.Hypergeometric
    (
      HypergeometricDistribution
    -- * Constructors
    , hypergeometric
    -- ** Accessors
    , hdM
    , hdL
    , hdK
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Numeric.MathFunctions.Constants (m_epsilon)
import Numeric.SpecFunctions (choose)
import qualified Statistics.Distribution as D
import Data.Binary (put, get)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif

data HypergeometricDistribution = HD {
      hdM :: {-# UNPACK #-} !Int
    , hdL :: {-# UNPACK #-} !Int
    , hdK :: {-# UNPACK #-} !Int
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON HypergeometricDistribution
instance ToJSON HypergeometricDistribution

instance Binary HypergeometricDistribution where
    get = HD <$> get <*> get <*> get
    put (HD x y z) = put x >> put y >> put z

instance D.Distribution HypergeometricDistribution where
    cumulative = cumulative

instance D.DiscreteDistr HypergeometricDistribution where
    probability = probability

instance D.Mean HypergeometricDistribution where
    mean = mean

instance D.Variance HypergeometricDistribution where
    variance = variance

instance D.MaybeMean HypergeometricDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance HypergeometricDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy HypergeometricDistribution where
  entropy = directEntropy

instance D.MaybeEntropy HypergeometricDistribution where
  maybeEntropy = Just . D.entropy

variance :: HypergeometricDistribution -> Double
variance (HD m l k) = (k' * ml) * (1 - ml) * (l' - k') / (l' - 1)
  where m' = fromIntegral m
        l' = fromIntegral l
        k' = fromIntegral k
        ml = m' / l'

mean :: HypergeometricDistribution -> Double
mean (HD m l k) = fromIntegral k * fromIntegral m / fromIntegral l

directEntropy :: HypergeometricDistribution -> Double
directEntropy d@(HD m _ _) =
    negate . sum $
  takeWhile (< negate m_epsilon) $
  dropWhile (not . (< negate m_epsilon)) $
  [ let x = probability d n in x * log x | n <- [0..m]]


hypergeometric :: Int               -- ^ /m/
               -> Int               -- ^ /l/
               -> Int               -- ^ /k/
               -> HypergeometricDistribution
hypergeometric m l k
  | not (l > 0)            = error $ msg ++ "l must be positive"
  | not (m >= 0 && m <= l) = error $ msg ++ "m must lie in [0,l] range"
  | not (k > 0 && k <= l)  = error $ msg ++ "k must lie in (0,l] range"
  | otherwise = HD m l k
    where
      msg = "Statistics.Distribution.Hypergeometric.hypergeometric: "

-- Naive implementation
probability :: HypergeometricDistribution -> Int -> Double
probability (HD mi li ki) n
  | n < max 0 (mi+ki-li) || n > min mi ki = 0
  | otherwise =
      choose mi n * choose (li - mi) (ki - n) / choose li ki

cumulative :: HypergeometricDistribution -> Double -> Double
cumulative d@(HD mi li ki) x
  | isNaN x      = error "Statistics.Distribution.Hypergeometric.cumulative: NaN argument"
  | isInfinite x = if x > 0 then 1 else 0
  | n <  minN    = 0
  | n >= maxN    = 1
  | otherwise    = D.sumProbabilities d minN n
  where
    n    = floor x
    minN = max 0 (mi+ki-li)
    maxN = min mi ki
