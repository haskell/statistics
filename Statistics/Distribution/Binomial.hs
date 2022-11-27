{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Binomial
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The binomial distribution.  This is the discrete probability
-- distribution of the number of successes in a sequence of /n/
-- independent yes\/no experiments, each of which yields success with
-- probability /p/.

module Statistics.Distribution.Binomial
    (
      BinomialDistribution
    -- * Constructors
    , binomial
    , binomialE
    -- * Accessors
    , bdTrials
    , bdProbability
    ) where

import Control.Applicative
import Data.Aeson            (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary           (Binary(..))
import Data.Data             (Data, Typeable)
import GHC.Generics          (Generic)
import Numeric.SpecFunctions           (choose,logChoose,incompleteBeta,log1p)
import Numeric.MathFunctions.Constants (m_epsilon,m_tiny)

import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Poisson.Internal as I
import Statistics.Internal


-- | The binomial distribution.
data BinomialDistribution = BD {
      bdTrials      :: {-# UNPACK #-} !Int
    -- ^ Number of trials.
    , bdProbability :: {-# UNPACK #-} !Double
    -- ^ Probability.
    } deriving (Eq, Typeable, Data, Generic)

instance Show BinomialDistribution where
  showsPrec i (BD n p) = defaultShow2 "binomial" n p i
instance Read BinomialDistribution where
  readPrec = defaultReadPrecM2 "binomial" binomialE

instance ToJSON BinomialDistribution
instance FromJSON BinomialDistribution where
  parseJSON (Object v) = do
    n <- v .: "bdTrials"
    p <- v .: "bdProbability"
    maybe (fail $ errMsg n p) return $ binomialE n p
  parseJSON _ = empty

instance Binary BinomialDistribution where
  put (BD x y) = put x >> put y
  get = do
    n <- get
    p <- get
    maybe (fail $ errMsg n p) return $ binomialE n p



instance D.Distribution BinomialDistribution where
    cumulative = cumulative
    complCumulative = complCumulative

instance D.DiscreteDistr BinomialDistribution where
    probability    = probability
    logProbability = logProbability

instance D.Mean BinomialDistribution where
    mean = mean

instance D.Variance BinomialDistribution where
    variance = variance

instance D.MaybeMean BinomialDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance BinomialDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy BinomialDistribution where
  entropy (BD n p)
    | n == 0 = 0
    | n <= 100 = directEntropy (BD n p)
    | otherwise = I.poissonEntropy (fromIntegral n * p)

instance D.MaybeEntropy BinomialDistribution where
  maybeEntropy = Just . D.entropy

-- This could be slow for big n
probability :: BinomialDistribution -> Int -> Double
probability (BD n p) k
  | k < 0 || k > n = 0
  | n == 0         = 1
    -- choose could overflow Double for n >= 1030 so we switch to
    -- log-domain to calculate probability
    --
    -- We also want to avoid underflow when computing p^k &
    -- (1-p)^(n-k).
  | n < 1000
  , pK  >= m_tiny
  , pNK >= m_tiny = choose n k * pK * pNK
  | otherwise     = exp $ logChoose n k + log p * k' + log1p (-p) * nk'
  where
    pK  = p^k
    pNK = (1-p)^(n-k)
    k'  = fromIntegral k
    nk' = fromIntegral $ n - k

logProbability :: BinomialDistribution -> Int -> Double
logProbability (BD n p) k
  | k < 0 || k > n          = (-1)/0
  | n == 0                  = 0
  | otherwise               = logChoose n k + log p * k' + log1p (-p) * nk'
  where
    k'  = fromIntegral   k
    nk' = fromIntegral $ n - k

cumulative :: BinomialDistribution -> Double -> Double
cumulative (BD n p) x
  | isNaN x      = error "Statistics.Distribution.Binomial.cumulative: NaN input"
  | isInfinite x = if x > 0 then 1 else 0
  | k <  0       = 0
  | k >= n       = 1
  | otherwise    = incompleteBeta (fromIntegral (n-k)) (fromIntegral (k+1)) (1 - p)
  where
    k = floor x

complCumulative :: BinomialDistribution -> Double -> Double
complCumulative (BD n p) x
  | isNaN x      = error "Statistics.Distribution.Binomial.complCumulative: NaN input"
  | isInfinite x = if x > 0 then 0 else 1
  | k <  0       = 1
  | k >= n       = 0
  | otherwise    = incompleteBeta (fromIntegral (k+1)) (fromIntegral (n-k)) p
  where
    k = floor x

mean :: BinomialDistribution -> Double
mean (BD n p) = fromIntegral n * p

variance :: BinomialDistribution -> Double
variance (BD n p) = fromIntegral n * p * (1 - p)

directEntropy :: BinomialDistribution -> Double
directEntropy d@(BD n _) =
  negate . sum $
  takeWhile (< negate m_epsilon) $
  dropWhile (not . (< negate m_epsilon)) $
  [ let x = probability d k in x * log x | k <- [0..n]]

-- | Construct binomial distribution. Number of trials must be
--   non-negative and probability must be in [0,1] range
binomial :: Int                 -- ^ Number of trials.
         -> Double              -- ^ Probability.
         -> BinomialDistribution
binomial n p = maybe (error $ errMsg n p) id $ binomialE n p

-- | Construct binomial distribution. Number of trials must be
--   non-negative and probability must be in [0,1] range
binomialE :: Int                 -- ^ Number of trials.
          -> Double              -- ^ Probability.
          -> Maybe BinomialDistribution
binomialE n p
  | n < 0            = Nothing
  | p >= 0 && p <= 1 = Just (BD n p)
  | otherwise        = Nothing

errMsg :: Int -> Double -> String
errMsg n p
  = "Statistics.Distribution.Binomial.binomial: n=" ++ show n
  ++ " p=" ++ show p ++ "but n>=0 and p in [0,1]"
