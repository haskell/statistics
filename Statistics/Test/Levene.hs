{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Statistics.Test.Levene
Description : Levene's test for homogeneity of variances.
Copyright   : (c) Praneya Kumar, Alexey Khudyakov, 2025
License     : BSD-3-Clause

Levene's test used to check whether samples have equal variance. Null
hypothesis is all samples are from distributions with same variance
(homoscedacity). Test is robust to non-normality, and versatile with
mean or median centering.

>>> import qualified Data.Vector.Unboxed as VU
>>> import Statistics.Test.Levene
>>> :{
let a = VU.fromList [8.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
    b = VU.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 8.36, 9.18, 8.67, 9.05]
    c = VU.fromList [8.95, 9.12, 8.95, 8.85, 9.03, 8.84, 9.07, 8.98, 8.86, 8.98]
in levenesTest Median [a, b, c]
:}
Right (Test {testSignificance = mkPValue 2.4315059672496814e-3, testStatistics = 7.584952754501659, testDistribution = fDistributionReal 2.0 27.0})
-}
module Statistics.Test.Levene (
    Center(..),
    levenesTest
) where

import Control.Monad
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as VU
import qualified Data.Vector.Generic   as VG
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector.Primitive as VP
#if MIN_VERSION_vector(0,13,2)
import qualified Data.Vector.Strict    as VV
#endif
import Statistics.Distribution (complCumulative)
import Statistics.Distribution.FDistribution (fDistribution, FDistribution)
import Statistics.Types      (mkPValue)
import Statistics.Test.Types (Test(..))
import Statistics.Function   (gsort)
import Statistics.Sample     (mean)

import qualified Statistics.Sample.Internal as IS
import Statistics.Quantile


-- | Center calculation method
data Center
  = Mean             -- ^ Use arithmetic mean
  | Median           -- ^ Use median
  | Trimmed !Double  -- ^ Trimmed mean with given proportion to cut from each end
  deriving (Eq, Show)

-- | Main Levene's test function with full error handling
levenesTest
  :: (VG.Vector v Double)
  => Center      -- ^ Centering method
  -> [v Double]  -- ^ Input samples
  -> Either String (Test FDistribution)
{-# INLINABLE levenesTest #-}
levenesTest center samples
  | length samples < 2 = Left "At least two samples required"
  -- NOTE: We don't have nice way of computing mean of a list!
  | otherwise = do
      let residuals = computeResiduals center <$> samples
      -- Average of all Z
      let n_tot = sum $ VG.length . vecZ <$> residuals -- Total number of samples
      let zbar = IS.sumF [ meanZ z * sampleN z
                         | z <- residuals]
               / fromIntegral n_tot
      -- Numerator: Sum over (ni * (Z[i] - Z)^2)
      let numerator = IS.sumF [ sampleN z * sqr (meanZ z - zbar)
                              | z <- residuals]
      -- Denominator: Sum over Σ((dev_ij - zbari)^2)
      let denominator = IS.sumF
            [ IS.sum $ VU.map (sqr . subtract (meanZ z)) (vecZ z)
            | z <- residuals
            ]
      -- Handle division by zero and invalid values
      when (denominator <= 0 || isNaN denominator || isInfinite denominator)
        $ Left "Invalid denominator in W-statistic calculation"
      let wStat = (fromIntegral (n_tot - k) / fromIntegral (k - 1)) * (numerator / denominator)
          fDist = fDistribution (k - 1) (n_tot - k)
      Right Test { testStatistics   = wStat
                 , testSignificance = mkPValue $ complCumulative fDist wStat
                 , testDistribution = fDist
                 }
  where
    k = length samples -- Number of groups
{-# SPECIALIZE levenesTest :: Center -> [V.Vector  Double] -> Either String (Test FDistribution) #-}
{-# SPECIALIZE levenesTest :: Center -> [VU.Vector Double] -> Either String (Test FDistribution) #-}
{-# SPECIALIZE levenesTest :: Center -> [VS.Vector Double] -> Either String (Test FDistribution) #-}
{-# SPECIALIZE levenesTest :: Center -> [VP.Vector Double] -> Either String (Test FDistribution) #-}
#if MIN_VERSION_vector(0,13,2)
{-# SPECIALIZE levenesTest :: Center -> [VV.Vector Double] -> Either String (Test FDistribution) #-}
#endif

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

-- | Trim data from both ends with error handling and performance optimization
trimboth :: (Ord a, Fractional a, VG.Vector v a)
         => v a
         -> Double
         -> v a
{-# INLINE trimboth #-}
trimboth vec p
  | p < 0 || p >= 0.5 = error "Statistics.Test.Levene: trimming: proportion must be between 0 and 0.5"
  | VG.null vec       = vec
  | otherwise         = VG.slice lowerCut (upperCut - lowerCut) sorted
  where
    n        = VG.length vec
    sorted   = gsort vec
    lowerCut = ceiling $ p * fromIntegral n
    upperCut = n - lowerCut

data Residuals = Residuals
  { sampleN :: !Double
  , meanZ   :: !Double
  , vecZ    :: !(VU.Vector Double)
  }

computeResiduals
  :: VG.Vector v Double
  => Center
  -> v Double
  -> Residuals
{-# INLINE computeResiduals #-}
computeResiduals method xs = case method of
  Mean   ->
    let c  = mean xs
        zs = VU.map (\x -> abs (x - c)) $ VU.convert xs
    in makeR zs
  Median ->
    let c  = median medianUnbiased xs
        zs = VU.map (\x -> abs (x - c)) $ VU.convert xs
    in makeR zs
  Trimmed p ->
    let trimmed = trimboth xs p
        c       = mean trimmed
        zs      = VU.map (\x -> abs (x - c)) $ VU.convert trimmed
    in makeR zs
  where
    makeR zs = Residuals { sampleN = fromIntegral $ VU.length zs
                         , meanZ   = mean zs
                         , vecZ    = zs
                         }

sqr :: Double -> Double
sqr x = x * x
