{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Statistics.Test.Bartlett
Description : Bartlett's test for homogeneity of variances.
Copyright   : (c) Praneya Kumar, Alexey Khudyakov, 2025
License     : BSD-3-Clause

Bartlett's test is used to check that multiple groups of observations
come from distributions with equal variances. This test assumes that
samples come from normal distribution. If this is not the case it may
simple test for non-normality and Levene's ("Statistics.Test.Levene")
is preferred

>>> import qualified Data.Vector.Unboxed as VU
>>> import Statistics.Test.Bartlett
>>> :{
let a = VU.fromList [8.88, 9.12, 9.04, 8.98, 9.00, 9.08, 9.01, 8.85, 9.06, 8.99]
    b = VU.fromList [8.88, 8.95, 9.29, 9.44, 9.15, 9.58, 8.36, 9.18, 8.67, 9.05]
    c = VU.fromList [8.95, 9.12, 8.95, 8.85, 9.03, 8.84, 9.07, 8.98, 8.86, 8.98]
in bartlettTest [a,b,c]
:}
Right (Test {testSignificance = mkPValue 1.1254782518843598e-5, testStatistics = 22.789434813726768, testDistribution = chiSquared 2})

-}
module Statistics.Test.Bartlett (
    bartlettTest,
    module Statistics.Distribution.ChiSquared
) where

import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as VU
import qualified Data.Vector.Generic   as VG
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector.Primitive as VP
#if MIN_VERSION_vector(0,13,2)
import qualified Data.Vector.Strict    as VV
#endif

import Statistics.Distribution (complCumulative)
import Statistics.Distribution.ChiSquared (chiSquared, ChiSquared(..))
import Statistics.Sample (varianceUnbiased)
import Statistics.Types (mkPValue)
import Statistics.Test.Types (Test(..))

-- | Perform Bartlett's test for equal variances. The input is a list
--   of vectors, where each vector represents a group of observations.
bartlettTest :: VG.Vector v Double => [v Double] -> Either String (Test ChiSquared)
bartlettTest groups
  | length groups < 2                 = Left "At least two groups are required for Bartlett's test."
  | any ((< 2) . VG.length) groups    = Left "Each group must have at least two observations."
  | any ((<= 0) . var) groupVariances = Left "All groups must have positive variance."
  | otherwise = Right Test
      { testSignificance = pValue
      , testStatistics   = tStatistic
      , testDistribution = chiDist
      }
  where
    -- Number of groups
    k = length groups
    -- Sample sizes for each group
    ni  = map (fromIntegral . VG.length) groups
    -- Total number of observations across all groups
    n_tot = sum $ fromIntegral . VG.length <$> groups
    -- Variance estimates
    groupVariances = toVar <$> groups
    sumWeightedVars = sum [ (n - 1) * v | Var{sampleN=n, var=v} <- groupVariances ]
    pooledVariance  = sumWeightedVars / fromIntegral (n_tot - k)
    -- Numerator of Bartlett's statistic
    numerator =
      fromIntegral (n_tot - k) * log pooledVariance -
      sum [ (n - 1) * log v | Var{sampleN=n, var=v} <- groupVariances ]
    -- Denominator correction term
    sumReciprocals = sum [1 / (n - 1) | n <- ni]
    denomCorrection =
      1 + (sumReciprocals - 1 / fromIntegral (n_tot - k)) / (3 * (fromIntegral k - 1))

    -- Test statistic and test distrubution
    tStatistic = max 0 $ numerator / denomCorrection
    chiDist    = chiSquared (k - 1)
    pValue     = mkPValue $ complCumulative chiDist tStatistic
{-# SPECIALIZE bartlettTest :: [V.Vector  Double] -> Either String (Test ChiSquared) #-}
{-# SPECIALIZE bartlettTest :: [VU.Vector Double] -> Either String (Test ChiSquared) #-}
{-# SPECIALIZE bartlettTest :: [VS.Vector Double] -> Either String (Test ChiSquared) #-}
{-# SPECIALIZE bartlettTest :: [VP.Vector Double] -> Either String (Test ChiSquared) #-}
#if MIN_VERSION_vector(0,13,2)
{-# SPECIALIZE bartlettTest :: [VV.Vector Double] -> Either String (Test ChiSquared) #-}
#endif

-- Estimate of variance
data Var = Var
  { sampleN :: !Double -- ^ N of elements
  , var     :: !Double -- ^ Sample variance
  }

toVar :: VG.Vector v Double => v Double -> Var
toVar xs = Var { sampleN = fromIntegral $ VG.length xs
               , var     = varianceUnbiased xs
               }
