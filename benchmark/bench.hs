import Data.Complex
import Statistics.Sample
import Statistics.Transform
import Statistics.Correlation
import System.Random.MWC
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

import Bench


-- Test sample
sample :: VU.Vector Double
sample = VU.create $ do g <- create
                        MVU.replicateM 10000 (uniform g)

-- Weighted test sample
sampleW :: VU.Vector (Double,Double)
sampleW = VU.zip sample (VU.reverse sample)

-- Complex vector for FFT tests
sampleC :: VU.Vector (Complex Double)
sampleC = VU.zipWith (:+) sample (VU.reverse sample)


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
      -- Correlation
    , bench "pearson"          $ nf pearson     sampleW
    , bench "covariance"       $ nf covariance  sampleW
    , bench "correlation"      $ nf correlation sampleW
    , bench "covariance2"      $ nf (covariance2  sample) sample
    , bench "correlation2"     $ nf (correlation2 sample) sample
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
      [ bench  (show n) $ whnf fft   (VU.take n sampleC) | n <- fftSizes ]
    , bgroup "ifft"
      [ bench  (show n) $ whnf ifft  (VU.take n sampleC) | n <- fftSizes ]
    , bgroup "dct"
      [ bench  (show n) $ whnf dct   (VU.take n sample)  | n <- fftSizes ]
    , bgroup "dct_"
      [ bench  (show n) $ whnf dct_  (VU.take n sampleC) | n <- fftSizes ]
    , bgroup "idct"
      [ bench  (show n) $ whnf idct  (VU.take n sample)  | n <- fftSizes ]
    , bgroup "idct_"
      [ bench  (show n) $ whnf idct_ (VU.take n sampleC) | n <- fftSizes ]
    ]
  ]


fftSizes :: [Int]
fftSizes = [32,128,512,2048]
