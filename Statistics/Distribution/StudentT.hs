{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
-- |
-- Module    : Statistics.Distribution.StudentT
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Student-T distribution
module Statistics.Distribution.StudentT (
    StudentT
  , studentT
  , studentTndf
  ) where

import qualified Statistics.Distribution as D
import Data.Typeable         (Typeable)
import Numeric.SpecFunctions (logBeta, incompleteBeta, invIncompleteBeta)



-- | Student-T distribution
newtype StudentT = StudentT { studentTndf :: Double }
                   deriving (Eq,Show,Read,Typeable)


-- | Create Student-T distribution. Number of parameters must be positive.
studentT :: Double -> StudentT
studentT ndf
  | ndf > 0   = StudentT ndf
  | otherwise =
    error "Statistics.Distribution.StudentT.studentT: non-positive number of degrees of freedom"

instance D.Distribution StudentT where
  type DistrSample StudentT = Double
  cumulative = cumulative 

instance D.ContDistr StudentT where
  density  = density
  quantile = quantile
  
cumulative :: StudentT -> Double -> Double
cumulative (StudentT ndf) x
  | x > 0     = 1 - 0.5 * ibeta
  | otherwise = 0.5 * ibeta
  where
    ibeta = incompleteBeta (0.5 * ndf) 0.5 (ndf / (ndf + x*x))

density :: StudentT -> Double -> Double
density (StudentT ndf) x =
    exp( log (ndf / (ndf + x*x)) * (0.5 * (1 + ndf)) - logBeta 0.5 (0.5 * ndf) ) / sqrt ndf

quantile :: StudentT -> Double -> Double
quantile (StudentT ndf) p
  | p >= 0 && p <= 1 = 
    let x = invIncompleteBeta (0.5 * ndf) 0.5 (2 * min p (1 - p))
    in case sqrt $ ndf * (1 - x) / x of
         r | p < 0.5   -> -r
           | otherwise -> r 
  | otherwise =
    error $ "Statistics.Distribution.Uniform.quantile: p must be in [0,1] range. Got: "++show p


instance D.MaybeMean StudentT where
  maybeMean (StudentT ndf) | ndf > 1   = Just 0
                           | otherwise = Nothing

instance D.MaybeVariance StudentT where
  maybeStdDev (StudentT ndf) | ndf > 2   = Just $ ndf / (ndf - 2)
                             | otherwise = Nothing

instance D.ContGen StudentT where
  genContVar = D.genContinous
