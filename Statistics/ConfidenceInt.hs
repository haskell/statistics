-- | Calculation of confidence intervals
module Statistics.ConfidenceInt (
    poissonCI
  , naiveBinomialCI
  ) where

import Data.Data   (Data,Typeable)
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Distribution.ChiSquared
import Statistics.Types



-- | Calculate confidence intervals for Poisson-distributed value for
--   single measurement. These are exact confidence intervals
poissonCI :: CL Double -> Int -> Estimate Double
poissonCI (CL cl) n
  | n <  0    = error "Statistics.ConfidenceInt.poissonErr: negative number of trials"
  | n == 0    = estimate m (0     , m2 - m) (CL cl)
  | otherwise = estimate m (m1 - m, m2 - m) (CL cl)
  where
    m  = fromIntegral n
    m1 = 0.5 * quantile (chiSquared (2*n  )) (cl/2)
    m2 = 0.5 * quantile (chiSquared (2*n+2)) (1 - cl/2)

-- | Calculate confidence interval using normal approximation. Note
--   that this approximation breaks down when /p/ is either close to 0
--   or to 1. In particular if @np < 5@ or @1 - np < 5@ this
--   approximation shouldn't be used.
naiveBinomialCI :: CL Double
                -> Int         -- ^ Number of trials
                -> Int         -- ^ Number of successes
                -> Estimate Double
naiveBinomialCI (CL cl) n k
  | n <= 0 || k < 0 = error "Statistics.ConfidenceInt.naiveBinomialCI: negative number of events"
  | k > n           = error "Statistics.ConfidenceInt.naiveBinomialCI: more successes than trials"
  | otherwise       = estimate eff (-dx,dx) (CL cl)
  where
    eff = fromIntegral k / fromIntegral n
    -- 1-sigma error. We need to scale it according to desired CL
    σ   = eff * (1 - eff) / fromIntegral n
    s   = negate $ quantile standard (cl / 2)
    dx  = σ * s

{-
Brown, Lawrence D.; Cai, T. Tony; DasGupta, Anirban
(2001). "Interval Estimation for a Binomial Proportion". Statistical
Science 16 (2): 101–133. doi:10.1214/ss/1009213286. MR 1861069. Zbl
02068924.
-}
