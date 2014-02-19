{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, CPP #-}
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
    -- * Accessors
    , bdTrials
    , bdProbability
    ) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Poisson.Internal as I
import Numeric.SpecFunctions (choose,incompleteBeta)
import Numeric.MathFunctions.Constants (m_epsilon)
#if !MIN_VERSION_binary(0, 6, 0)
import Data.Binary (put, get)
import Control.Applicative ((<$>), (<*>))
#endif


-- | The binomial distribution.
data BinomialDistribution = BD {
      bdTrials      :: {-# UNPACK #-} !Int
    -- ^ Number of trials.
    , bdProbability :: {-# UNPACK #-} !Double
    -- ^ Probability.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary BinomialDistribution where
#if !MIN_VERSION_binary(0, 6, 0)
    put (BD x y) = put x >> put y
    get = BD <$> get <*> get
#endif

instance D.Distribution BinomialDistribution where
    cumulative = cumulative

instance D.DiscreteDistr BinomialDistribution where
    probability = probability

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
  | otherwise      = choose n k * p^k * (1-p)^(n-k)
{-# INLINE probability #-}

-- Summation from different sides required to reduce roundoff errors
cumulative :: BinomialDistribution -> Double -> Double
cumulative (BD n p) x
  | isNaN x      = error "Statistics.Distribution.Binomial.cumulative: NaN input"
  | isInfinite x = if x > 0 then 1 else 0
  | k <  0       = 0
  | k >= n       = 1
  | otherwise    = incompleteBeta (fromIntegral (n-k)) (fromIntegral (k+1)) (1 - p)
  where
    k = floor x
{-# INLINE cumulative #-}

mean :: BinomialDistribution -> Double
mean (BD n p) = fromIntegral n * p
{-# INLINE mean #-}

variance :: BinomialDistribution -> Double
variance (BD n p) = fromIntegral n * p * (1 - p)
{-# INLINE variance #-}

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
binomial n p
  | n < 0          =
    error $ msg ++ "number of trials must be non-negative. Got " ++ show n
  | p < 0 || p > 1 =
    error $ msg++"probability must be in [0,1] range. Got " ++ show p
  | otherwise      = BD n p
    where msg = "Statistics.Distribution.Binomial.binomial: "
{-# INLINE binomial #-}
