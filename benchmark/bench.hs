import Control.Monad.ST (runST)
import Criterion.Main
import Data.Complex
import Statistics.Sample
import Statistics.Transform
import System.Random.MWC
import qualified Data.Vector.Unboxed as U


-- Test sample
sample :: U.Vector Double
sample = runST $ flip uniformVector 10000 =<< create

-- Weighted test sample
sampleW :: U.Vector (Double,Double)
sampleW = U.zip sample (U.reverse sample)

-- Comlex vector for FFT tests
sampleC :: U.Vector (Complex Double)
sampleC = U.zipWith (:+) sample (U.reverse sample)


-- Simple benchmark for functions from Statistics.Sample
main :: IO ()
main =
  defaultMain
  [ bgroup "sample"
    [ bench "range"            $ nf (\x -> range x)            sample
      -- Mean
    , bench "mean"             $ nf (\x -> mean x)             sample
    , bench "meanWeighted"     $ nf (\x -> meanWeighted x)     sampleW
    , bench "harmonicMean"     $ nf (\x -> harmonicMean x)     sample
    , bench "geometricMean"    $ nf (\x -> geometricMean x)    sample
      -- Variance
    , bench "variance"         $ nf (\x -> variance x)         sample
    , bench "varianceUnbiased" $ nf (\x -> varianceUnbiased x) sample
    , bench "varianceWeighted" $ nf (\x -> varianceWeighted x) sampleW
      -- Other
    , bench "stdDev"           $ nf (\x -> stdDev x)           sample
    , bench "skewness"         $ nf (\x -> skewness x)         sample
    , bench "kurtosis"         $ nf (\x -> kurtosis x)         sample
      -- Central moments
    , bench "C.M. 2"           $ nf (\x -> centralMoment 2 x)  sample
    , bench "C.M. 3"           $ nf (\x -> centralMoment 3 x)  sample
    , bench "C.M. 4"           $ nf (\x -> centralMoment 4 x)  sample
    , bench "C.M. 5"           $ nf (\x -> centralMoment 5 x)  sample
    ]
  , bgroup "FFT"
    [ bgroup "fft"
      [ bench  (show n) $ whnf fft   (U.take n sampleC) | n <- fftSizes ]
    , bgroup "ifft"
      [ bench  (show n) $ whnf ifft  (U.take n sampleC) | n <- fftSizes ]
    , bgroup "dct"
      [ bench  (show n) $ whnf dct   (U.take n sample)  | n <- fftSizes ]
    , bgroup "dct_"
      [ bench  (show n) $ whnf dct_  (U.take n sampleC) | n <- fftSizes ]
    , bgroup "idct"
      [ bench  (show n) $ whnf idct  (U.take n sample)  | n <- fftSizes ]
    , bgroup "idct_"
      [ bench  (show n) $ whnf idct_ (U.take n sampleC) | n <- fftSizes ]
    ]
  ]


fftSizes :: [Int]
fftSizes = [32,128,512,2048]
