module Statistics.Distribution.Normal
    (
      NormalDistribution
    , fromParams
    , fromSample
    , standard
    ) where

import Control.Exception (assert)
import Data.Number.Erf (erfc)
import Statistics.Internal (huge, sqrt_2, sqrt_2_pi)
import qualified Statistics.Distribution as D
import qualified Statistics.Sample as S

data NormalDistribution = NormalDistribution {
      mean     :: {-# UNPACK #-} !Double
    , variance :: {-# UNPACK #-} !Double
    , pdfDenom :: {-# UNPACK #-} !Double
    , cdfDenom :: {-# UNPACK #-} !Double
    } deriving (Eq, Ord, Read, Show)

instance D.Distribution NormalDistribution where
    probability = probability
    cumulative  = cumulative
    inverse     = inverse

standard :: NormalDistribution
standard = NormalDistribution {
             mean = 0.0
           , variance = 1.0
           , cdfDenom = sqrt_2
           , pdfDenom = sqrt_2_pi
           }

fromParams :: Double -> Double -> NormalDistribution
fromParams m v = assert (v > 0) $
                 NormalDistribution {
                   mean = m
                 , variance = v
                 , cdfDenom = sqrt_2 * sv
                 , pdfDenom = sqrt_2_pi * sv
                 }
    where sv = sqrt v
                   
fromSample :: S.Sample -> NormalDistribution
fromSample a = fromParams (S.mean a) (S.variance a)

probability :: NormalDistribution -> Double -> Double
probability d x = exp (-xm * xm / (2 * variance d)) / pdfDenom d
    where xm = x - mean d

cumulative :: NormalDistribution -> Double -> Double
cumulative d x = erfc (-(x-mean d) / cdfDenom d) / 2

inverse :: NormalDistribution -> Double -> Double
inverse d p
  | p == 0    = -huge
  | p == 1    = huge
  | p == 0.5  = mean d
  | otherwise = x * sqrt (variance d) + mean d
  where x     = D.findRoot standard p 0 (-100) 100
