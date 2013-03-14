{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution.StudentT
-- Copyright : (c) 2011 Aleksey Khudyakov; 2013 John McDonnell
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
  , StudentTGeneral
  , studentTGeneral
  , studentTGeneralndf
  , studentTGeneralmu
  , studentTGeneralsigma
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
  maybeVariance (StudentT ndf) | ndf > 2   = Just $ ndf / (ndf - 2)
                               | ndf > 1   = Just inf
                               | otherwise = Nothing
    where inf = 1/0

instance D.ContGen StudentT where
  genContVar = D.genContinous

-- | Student-T distribution generalized to the unnormalized case
data StudentTGeneral = StudentTGeneral { studentTGeneralndf   :: {-# UNPACK #-} !Double
                                       , studentTGeneralmu    :: {-# UNPACK #-} !Double 
                                       , studentTGeneralsigma :: {-# UNPACK #-} !Double 
                                       }
                                 deriving (Eq,Show,Read,Typeable)


-- | Create Student-T distribution. Number of parameters must be positive.
studentTGeneral :: Double -> Double -> Double -> StudentTGeneral
studentTGeneral ndf mu sigma
  | ndf <= 0 = error $ fname ++ ": degrees of freedom <= 0"
  | sigma <= 0 = error $ fname ++ ": sigma <= 0"
  | otherwise = StudentTGeneral ndf mu sigma
  where 
    fname = "Statistics.Distribution.StudentT.studentTGeneral"

instance D.Distribution StudentTGeneral where
  cumulative = cumulativeGeneral

instance D.ContDistr StudentTGeneral where
  density  = densityGeneral
  quantile = quantileGeneral
  
cumulativeGeneral :: StudentTGeneral -> Double -> Double
cumulativeGeneral (StudentTGeneral ndf mu sigma) x = cumulative simplestudent arg
  where
    arg = (x-mu) / sigma
    simplestudent = studentT ndf

sq :: Double -> Double
sq x = x*x

densityGeneral :: StudentTGeneral -> Double -> Double
densityGeneral (StudentTGeneral ndf mu sigma) x = (density simplestudent arg) /sigma
  where
    arg = (x-mu) / sigma
    simplestudent = studentT ndf

quantileGeneral :: StudentTGeneral -> Double -> Double
quantileGeneral (StudentTGeneral ndf mu sigma) p = mu + sigma * (quantile simplestudent p)
  where
    simplestudent = studentT ndf


instance D.MaybeMean StudentTGeneral where
  maybeMean (StudentTGeneral ndf mu _) 
    | ndf > 1   = Just mu
    | otherwise = Nothing

instance D.MaybeVariance StudentTGeneral where
  maybeVariance  (StudentTGeneral ndf _ sigma) 
    | ndf > 2   = Just $ (sq sigma) * (ndf / (ndf - 2))
    | ndf > 1   = Just inf
    | otherwise = Nothing
      where inf = 1/0
  maybeStdDev  (StudentTGeneral ndf _ sigma) 
    | ndf > 2   = Just $ sigma * (sqrt (ndf / (ndf - 2)))
    | ndf > 1   = Just inf
    | otherwise = Nothing
      where inf = 1/0

instance D.ContGen StudentTGeneral where
  genContVar = D.genContinous
