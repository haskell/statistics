-- |
-- Module    : Statistics.Resampling.Bootstrap
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The bootstrap method for statistical inference.

module Statistics.Resampling.Bootstrap
    (
      Resample
    , doResampling
    , fromResample
    , bootstrapBCA
    ) where

import Control.Exception (assert)
import Control.Monad (forM_)
import Control.Monad.ST (unsafeSTToIO)
import Data.Array.Vector
import System.Random.Mersenne (MTGen, random)
import Data.Array.Vector.Algorithms.Intro (sort)
import Statistics.Distribution.Normal hiding (mean)
import Statistics.Distribution (cumulative, inverse)
import Statistics.Sample (mean)

-- | A function that estimates a property of a sample, such as 'mean'.
type Estimator = Sample -> Double

newtype Resample = Resample {
      fromResample :: UArr Double
    } deriving (Eq, Show)

createU :: (UA e) => Int -> (Int -> IO e) -> IO (UArr e)
createU size itemAt = assert (size >= 0) $
    unsafeSTToIO (newMU size) >>= loop 0
  where
    loop k arr | k >= size = unsafeSTToIO (unsafeFreezeAllMU arr)
               | otherwise = do
      r <- itemAt k
      unsafeSTToIO (writeMU arr k r)
      loop (k+1) arr

resample :: MTGen -> Sample -> IO Resample
resample gen values = fmap Resample . createU n $ \_ -> do
    r <- random gen
    return (indexU values (abs r `mod` n))
  where n = lengthU values

type Sample = UArr Double

doResampling :: MTGen -> [Estimator] -> Int -> Sample
             -> IO [Resample]
doResampling gen ests numResamples samples = do
  results <- unsafeSTToIO . mapM (const (newMU numResamples)) $ ests
  loop 0 (zip ests results)
  unsafeSTToIO $ do
    mapM_ sort results
    mapM (fmap Resample . unsafeFreezeAllMU) results
 where
  loop k ers | k >= numResamples = return ()
             | otherwise = do
    r <- resample gen samples
    unsafeSTToIO . forM_ ers $ \(est,arr) ->
        writeMU arr k (est (fromResample r))
    loop (k+1) ers

data Estimate = Estimate {
      estPoint :: !Double
    , estLowerBound :: !Double
    , estUpperBound :: !Double
    , estConfidenceLevel :: !Double
    } deriving (Eq, Show)

data T = {-# UNPACK #-} !Double :< {-# UNPACK #-} !Double
infixl 2 :<

bootstrapBCA :: Double          -- ^ Confidence level
             -> Sample          -- ^ Sample data
             -> [Estimator]     -- ^ Estimators
             -> [Resample]      -- ^ Resampled data
             -> [Estimate]
bootstrapBCA confidenceLevel sample = zipWith e
  where
    e est (Resample resample) =
        Estimate {
            estPoint = pt
          , estLowerBound = indexU resample lo
          , estUpperBound = indexU resample hi
          , estConfidenceLevel = confidenceLevel
          }
      where
        pt    = est sample
        lo    = max (cumn a1) 0
          where a1 = bias + (b1 / (1 - accel * b1))
                b1 = bias + z1
        hi    = min (cumn a2) (ni - 1)
          where a2 = bias + (b2 / (1 - accel * b2))
                b2 = bias - z1
        z1    = inverse standard ((1 - confidenceLevel) / 2)
        cumn  = round . (*n) . cumulative standard
        bias  = inverse standard (probN / n)
          where probN = fromIntegral . lengthU . filterU (<pt) $ resample
        ni    = lengthU resample
        n     = fromIntegral ni
        accel = sumCubes / (6 * (sumSquares ** 1.5))
          where (sumSquares :< sumCubes) = foldlU f (0 :< 0) jack
                f (s :< c) j = s + d2 :< c + d2 * d
                    where d  = jackMean - j
                          d2 = d * d
                jackMean     = mean jack
        jack  = mapU f . enumFromToU 0 . subtract 1 . lengthU $ sample
          where f i = est (a `appendU` tailU b)
                  where (a,b) = splitAtU i sample
