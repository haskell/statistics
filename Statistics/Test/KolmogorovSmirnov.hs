module Statistics.Test.KolmogorovSmirnov where

import qualified Data.Vector.Unboxed as U

import Statistics.Distribution        (Distribution(..))
import Statistics.Types               (Sample)
import Statistics.Function            (sort)


-- | Calculate /D/ for given CDF and sample
kolmogorovSmirnovCdfD :: (Double -> Double) -- ^ CDF function
                      -> Sample             -- ^ Sample
                      -> Double
kolmogorovSmirnovCdfD cdf sample
  | U.null xs = 0
  | otherwise = U.maximum
              $ U.zipWith3 (\p a b -> max (abs (p-a)) (abs (p-b)))
                  ps steps (U.tail steps)
  where
    xs = sort sample
    n  = U.length xs
    --
    ps    = U.map cdf xs
    steps = U.map ((/ fromIntegral n) . fromIntegral) 
          $ U.generate (n+1) id


-- | Calculate /D/ for given distribution and sample
kolmogorovSmirnovD :: (Distribution d)
                   => d         -- ^ Distribution
                   -> Sample    -- ^ Sample
                   -> Double
kolmogorovSmirnovD d = kolmogorovSmirnovCdfD (cumulative d)
{-# INLINE kolmogorovSmirnovD #-}
