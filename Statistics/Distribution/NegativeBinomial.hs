{-# LANGUAGE OverloadedStrings, PatternGuards,
             DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.NegativeBinomial
-- Copyright : (c) 2022 Lorenz Minder
-- License   : BSD3
--
-- Maintainer  : lminder@gmx.net
-- Stability   : experimental
-- Portability : portable
--
-- The negative binomial distribution.  This is the discrete probability
-- distribution of the number of failures in a sequence of independent
-- yes\/no experiments before a specified number of successes /r/.  Each
-- Bernoulli trial has success probability /p/ in the range (0, 1].  The
-- parameter /r/ must be positive, but does not have to be integer.

module Statistics.Distribution.NegativeBinomial (
      NegativeBinomialDistribution
    -- * Constructors
    , negativeBinomial
    , negativeBinomialE
    -- * Accessors
    , nbdSuccesses
    , nbdProbability
) where

import Control.Applicative
import Data.Aeson                       (FromJSON(..), ToJSON, Value(..), (.:))
import Data.Binary                      (Binary(..))
import Data.Data                        (Data, Typeable)
import Data.Foldable                    (foldl')
import GHC.Generics                     (Generic)
import Numeric.SpecFunctions            (incompleteBeta, log1p)
import Numeric.SpecFunctions.Extra      (logChooseFast)
import Numeric.MathFunctions.Constants  (m_epsilon, m_tiny)

import qualified Statistics.Distribution as D
import Statistics.Internal

-- Math helper functions

-- | Generalized binomial coefficients.
--
--   These computes binomial coefficients with the small generalization
--   that the /n/ need not be integer, but can be real.
gChoose :: Double -> Int -> Double
gChoose n k
    | k < 0             = 0
    | k' >= 50          = exp $ logChooseFast n k' 
    | otherwise         = foldl' (*) 1 factors
    where   factors = [ (n - k' + j) / j | j <- [1..k'] ]
            k' = fromIntegral k


-- Implementation of Negative Binomial

-- | The negative binomial distribution.
data NegativeBinomialDistribution = NBD {
      nbdSuccesses   :: {-# UNPACK #-} !Double
    -- ^ Number of successes until stop
    , nbdProbability :: {-# UNPACK #-} !Double
    -- ^ Success probability.
    } deriving (Eq, Typeable, Data, Generic)

instance Show NegativeBinomialDistribution where
  showsPrec i (NBD r p) = defaultShow2 "negativeBinomial" r p i
instance Read NegativeBinomialDistribution where
  readPrec = defaultReadPrecM2 "negativeBinomial" negativeBinomialE

instance ToJSON NegativeBinomialDistribution
instance FromJSON NegativeBinomialDistribution where
  parseJSON (Object v) = do
    r <- v .: "nbdSuccesses"
    p <- v .: "nbdProbability"
    maybe (fail $ errMsg r p) return $ negativeBinomialE r p
  parseJSON _ = empty

instance Binary NegativeBinomialDistribution where
  put (NBD r p) = put r >> put p
  get = do
    r <- get
    p <- get
    maybe (fail $ errMsg r p) return $ negativeBinomialE r p

instance D.Distribution NegativeBinomialDistribution where
    cumulative = cumulative
    complCumulative = complCumulative

instance D.DiscreteDistr NegativeBinomialDistribution where
    probability    = probability
    logProbability = logProbability

instance D.Mean NegativeBinomialDistribution where
    mean = mean

instance D.Variance NegativeBinomialDistribution where
    variance = variance

instance D.MaybeMean NegativeBinomialDistribution where
    maybeMean = Just . D.mean

instance D.MaybeVariance NegativeBinomialDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Entropy NegativeBinomialDistribution where
   entropy = directEntropy

instance D.MaybeEntropy NegativeBinomialDistribution where
   maybeEntropy = Just . D.entropy

-- This could be slow for big n
probability :: NegativeBinomialDistribution -> Int -> Double
probability d@(NBD r p) k
  | k < 0          = 0
    -- Switch to log domain for large k + r to avoid overflows.
    --
    -- We also want to avoid underflow when computing (1-p)^k &
    -- p^r.
  | k' + r < 1000
  , pK >= m_tiny
  , pR >= m_tiny  = gChoose (k' + r - 1) k * pK * pR
  | otherwise     = exp $ logProbability d k
  where
    pK  = exp $ log1p (-p) * k'
    pR  = p**r
    k'  = fromIntegral k

logProbability :: NegativeBinomialDistribution -> Int -> Double
logProbability (NBD r p) k
  | k < 0                   = (-1)/0
  | otherwise               = logChooseFast (k' + r - 1) k'
                              + log1p (-p) * k'
                              + log p * r
  where k' = fromIntegral k

cumulative :: NegativeBinomialDistribution -> Double -> Double
cumulative (NBD r p) x
  | isNaN x      = error "Statistics.Distribution.NegativeBinomial.cumulative: NaN input"
  | isInfinite x = if x > 0 then 1 else 0
  | k < 0        = 0
  | otherwise    = incompleteBeta r (fromIntegral (k+1)) p
  where
    k = floor x :: Integer

complCumulative :: NegativeBinomialDistribution -> Double -> Double
complCumulative (NBD r p) x
  | isNaN x      = error "Statistics.Distribution.NegativeBinomial.complCumulative: NaN input"
  | isInfinite x = if x > 0 then 0 else 1
  | k < 0        = 1
  | otherwise    = incompleteBeta (fromIntegral (k+1)) r (1 - p)
  where
    k = (floor x)::Integer

mean :: NegativeBinomialDistribution -> Double
mean (NBD r p) = r * (1 - p)/p

variance :: NegativeBinomialDistribution -> Double
variance (NBD r p) = r * (1 - p)/(p * p)

directEntropy :: NegativeBinomialDistribution -> Double
directEntropy d =
  negate . sum $
  takeWhile (< -m_epsilon) $
  dropWhile (>= -m_epsilon) $
  [ let x = probability d k in x * log x | k <- [0..]]

-- | Construct negative binomial distribution. Number of failures /r/
--   must be positive and probability must be in (0,1] range
negativeBinomial :: Double              -- ^ Number of successes.
                 -> Double              -- ^ Success probability.
                 -> NegativeBinomialDistribution
negativeBinomial r p = maybe (error $ errMsg r p) id $ negativeBinomialE r p

-- | Construct negative binomial distribution. Number of failures /r/
--   must be positive and probability must be in (0,1] range
negativeBinomialE :: Double              -- ^ Number of successes.
                  -> Double              -- ^ Success probability.
                  -> Maybe NegativeBinomialDistribution
negativeBinomialE r p
  | r > 0 && 0 < p && p <= 1            = Just (NBD r p)
  | otherwise                           = Nothing

errMsg :: Double -> Double -> String
errMsg r p
  = "Statistics.Distribution.NegativeBinomial.negativeBinomial: r=" ++ show r
  ++ " p=" ++ show p ++ ", but need r>0 and p in (0,1]"
