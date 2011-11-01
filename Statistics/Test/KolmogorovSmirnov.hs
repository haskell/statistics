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

-- | Calculate /D/ statistics for two samples
kolmogorovSmirnov2D :: Sample   -- ^ First sample
                    -> Sample   -- ^ Second sample
                    -> Double
kolmogorovSmirnov2D sample1 sample2
  | U.null sample1 || U.null sample2 = 0
  | otherwise                        = worker 0 0 0
  where
    xs1 = sort sample1
    xs2 = sort sample2
    n1  = U.length xs1
    n2  = U.length xs2
    en1 = fromIntegral n1
    en2 = fromIntegral n2
    -- Find new index
    skip x i xs = go (i+1)
      where go n | n >= U.length xs = n
                 | xs U.! n == x    = go (n+1)
                 | otherwise        = n
    -- Main loop
    worker d i1 i2
      | i1 >= n1 || i2 >= n2 = d
      | otherwise            = worker d' i1' i2'
      where
        d1  = xs1 U.! i1
        d2  = xs2 U.! i2
        i1' | d1 <= d2  = skip d1 i1 xs1
            | otherwise = i1
        i2' | d2 <= d1  = skip d2 i2 xs2
            | otherwise = i2
        d'  = max d (abs $ fromIntegral i1' / en1 - fromIntegral i2' / en2)
