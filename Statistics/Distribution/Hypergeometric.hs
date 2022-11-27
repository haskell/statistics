{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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
    , hypergeometricE
    -- ** Accessors
    , hdM
    , hdL
    , hdK
    ) where

import Control.Applicative
import Data.Aeson           (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary          (Binary(..))
import Data.Data            (Data, Typeable)
import GHC.Generics         (Generic)
import Numeric.MathFunctions.Constants (m_epsilon,m_neg_inf)
import Numeric.SpecFunctions (choose,logChoose)

import qualified Statistics.Distribution as D
import Statistics.Internal


data HypergeometricDistribution = HD {
      hdM :: {-# UNPACK #-} !Int
    , hdL :: {-# UNPACK #-} !Int
    , hdK :: {-# UNPACK #-} !Int
    } deriving (Eq, Typeable, Data, Generic)

instance Show HypergeometricDistribution where
  showsPrec i (HD m l k) = defaultShow3 "hypergeometric" m l k i
instance Read HypergeometricDistribution where
  readPrec = defaultReadPrecM3 "hypergeometric" hypergeometricE

instance ToJSON HypergeometricDistribution
instance FromJSON HypergeometricDistribution where
  parseJSON (Object v) = do
    m <- v .: "hdM"
    l <- v .: "hdL"
    k <- v .: "hdK"
    maybe (fail $ errMsg m l k) return $ hypergeometricE m l k
  parseJSON _ = empty

instance Binary HypergeometricDistribution where
  put (HD m l k) = put m >> put l >> put k
  get = do
    m <- get
    l <- get
    k <- get
    maybe (fail $ errMsg m l k) return $ hypergeometricE m l k

instance D.Distribution HypergeometricDistribution where
    cumulative = cumulative
    complCumulative = complCumulative

instance D.DiscreteDistr HypergeometricDistribution where
    probability    = probability
    logProbability = logProbability

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
directEntropy d@(HD m _ _)
  = negate . sum
  $ takeWhile (< negate m_epsilon)
  $ dropWhile (not . (< negate m_epsilon))
    [ let x = probability d n in x * log x | n <- [0..m]]


hypergeometric :: Int               -- ^ /m/
               -> Int               -- ^ /l/
               -> Int               -- ^ /k/
               -> HypergeometricDistribution
hypergeometric m l k
  = maybe (error $ errMsg m l k) id $ hypergeometricE m l k

hypergeometricE :: Int               -- ^ /m/
                -> Int               -- ^ /l/
                -> Int               -- ^ /k/
                -> Maybe HypergeometricDistribution
hypergeometricE m l k
  | not (l > 0)            = Nothing
  | not (m >= 0 && m <= l) = Nothing
  | not (k > 0  && k <= l) = Nothing
  | otherwise              = Just (HD m l k)


errMsg :: Int -> Int -> Int -> String
errMsg m l k
  =  "Statistics.Distribution.Hypergeometric.hypergeometric:"
  ++ " m=" ++ show m
  ++ " l=" ++ show l
  ++ " k=" ++ show k
  ++ " should hold: l>0 & m in [0,l] & k in (0,l]"

-- Naive implementation
probability :: HypergeometricDistribution -> Int -> Double
probability (HD mi li ki) n
  | n < max 0 (mi+ki-li) || n > min mi ki = 0
    -- No overflow
  | li < 1000 = choose mi n * choose (li - mi) (ki - n)
              / choose li ki
  | otherwise = exp $ logChoose mi n
                    + logChoose (li - mi) (ki - n)
                    - logChoose li ki

logProbability :: HypergeometricDistribution -> Int -> Double
logProbability (HD mi li ki) n
  | n < max 0 (mi+ki-li) || n > min mi ki = m_neg_inf
  | otherwise = logChoose mi n
              + logChoose (li - mi) (ki - n)
              - logChoose li ki

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

complCumulative :: HypergeometricDistribution -> Double -> Double
complCumulative d@(HD mi li ki) x
  | isNaN x      = error "Statistics.Distribution.Hypergeometric.complCumulative: NaN argument"
  | isInfinite x = if x > 0 then 0 else 1
  | n <  minN    = 1
  | n >= maxN    = 0
  | otherwise    = D.sumProbabilities d (n + 1) maxN
  where
    n    = floor x
    minN = max 0 (mi+ki-li)
    maxN = min mi ki
