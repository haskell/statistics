-- |
-- Module    : Statistics.Resampling
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Resampling statistics.

module Statistics.Resampling
    (
      Resample(..)
    , jackknife
    , resample
    ) where

import Control.Exception (assert)
import Control.Monad (forM_)
import Control.Monad.ST (unsafeSTToIO)
import Data.Array.Vector
import Data.Array.Vector.Algorithms.Intro (sort)
import Statistics.Types (Estimator, Sample)
import System.Random.Mersenne (MTGen, random)

newtype Resample = Resample {
      fromResample :: UArr Double
    } deriving (Eq, Show)

resample :: MTGen -> [Estimator] -> Int -> Sample -> IO [Resample]
resample gen ests numResamples samples = do
  results <- unsafeSTToIO . mapM (const (newMU numResamples)) $ ests
  loop 0 (zip ests results)
  unsafeSTToIO $ do
    mapM_ sort results
    mapM (fmap Resample . unsafeFreezeAllMU) results
 where
  loop k ers | k >= numResamples = return ()
             | otherwise = do
    r <- oneResample gen samples
    unsafeSTToIO . forM_ ers $ \(est,arr) ->
        writeMU arr k (est (fromResample r))
    loop (k+1) ers
  oneResample gen values = fmap Resample . createU n $ \_ -> do
      r <- random gen
      return (indexU values (abs r `mod` n))
    where n = lengthU values

createU :: (UA e) => Int -> (Int -> IO e) -> IO (UArr e)
createU size itemAt = assert (size >= 0) $
    unsafeSTToIO (newMU size) >>= loop 0
  where
    loop k arr | k >= size = unsafeSTToIO (unsafeFreezeAllMU arr)
               | otherwise = do
      r <- itemAt k
      unsafeSTToIO (writeMU arr k r)
      loop (k+1) arr

-- | Compute a statistical estimate repeatedly over a sample, each
-- time omitting a successive element.
jackknife :: Estimator -> Sample -> UArr Double
jackknife est sample = mapU f . enumFromToU 0 . subtract 1 . lengthU $ sample
    where f i = est (dropAt i sample)
{-# INLINE jackknife #-}

-- | Drop the /k/th element of a vector.
dropAt :: UA e => Int -> UArr e -> UArr e
dropAt n = mapU sndT . filterU notN . indexedU
    where notN (i :*: _) = i /= n
          sndT (_ :*: k) = k
