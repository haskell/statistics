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

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.Vector
import Data.Array.Vector.Algorithms.Intro (sort)
import Statistics.Function (createU, indices)
import System.Random.MWC (Gen, uniform)
import Statistics.Types (Estimator, Sample)

-- | A resample drawn randomly, with replacement, from a set of data
-- points.  Distinct from a normal array to make it harder for your
-- humble author's brain to go wrong.
newtype Resample = Resample {
      fromResample :: UArr Double
    } deriving (Eq, Show)

-- | Resample a data set repeatedly, with replacement, computing each
-- estimate over the resampled data.
resample :: Gen s -> [Estimator] -> Int -> Sample -> ST s [Resample]
resample gen ests numResamples samples = do
  results <- mapM (const (newMU numResamples)) $ ests
  loop 0 (zip ests results)
  mapM_ sort results
  mapM (fmap Resample . unsafeFreezeAllMU) results
 where
  loop k ers | k >= numResamples = return ()
             | otherwise = do
    re <- createU n $ \_ -> do
            r <- uniform gen
            return (indexU samples (abs r `mod` n))
    forM_ ers $ \(est,arr) ->
        writeMU arr k . est $ re
    loop (k+1) ers
  n = lengthU samples

-- | Compute a statistical estimate repeatedly over a sample, each
-- time omitting a successive element.
jackknife :: Estimator -> Sample -> UArr Double
jackknife est sample = mapU f . indices $ sample
    where f i = est (dropAt i sample)
{-# INLINE jackknife #-}

-- | Drop the /k/th element of a vector.
dropAt :: UA e => Int -> UArr e -> UArr e
dropAt n = mapU sndT . filterU notN . indexedU
    where notN (i :*: _) = i /= n
          sndT (_ :*: k) = k
