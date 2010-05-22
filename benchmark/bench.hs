
import Control.Monad.ST (runST)

import System.Random.MWC
import qualified Data.Vector.Unboxed as U
import Criterion.Main

import Statistics.Sample

-- Test sample
sample :: U.Vector Double
sample = runST $ flip uniformVector 10000 =<< create

-- Weighted test sample
sampleW :: U.Vector (Double,Double)
sampleW = U.zip sample (U.reverse sample)

-- Simple benchmark for functions from Statistics.Sample
main :: IO ()
main = defaultMain [ bench "range"            $ nf range            sample
                   -- Mean
                   , bench "mean"             $ nf mean             sample
                   , bench "meanWeighted"     $ nf meanWeighted     sampleW
                   , bench "harmonicMean"     $ nf harmonicMean     sample
                   , bench "geometricMean"    $ nf geometricMean    sample
                   -- Variance
                   , bench "variance"         $ nf variance         sample
                   , bench "varianceUnbiased" $ nf varianceUnbiased sample
                   , bench "varianceWeighted" $ nf varianceWeighted sampleW
                   -- Other
                   , bench "stdDev"           $ nf stdDev           sample
                   , bench "skewness"         $ nf skewness         sample
                   , bench "kurtosis"         $ nf kurtosis         sample
                   -- Central moments
                   , bench "C.M. 2"           $ nf (centralMoment 2)  sample
                   , bench "C.M. 3"           $ nf (centralMoment 3)  sample
                   , bench "C.M. 4"           $ nf (centralMoment 4)  sample
                   , bench "C.M. 5"           $ nf (centralMoment 5)  sample
                   ]
