-- |
-- Module    : Statistics.Resampling
-- Copyright : (c) 2009, 2010 Bryan O'Sullivan
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

import Control.Monad (forM_, liftM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed ((!))
import Data.Vector.Generic (unsafeFreeze)
import Data.Vector.Algorithms.Intro (sort)
import Statistics.Function (create, indexed, indices)
import System.Random.MWC (Gen, uniform)
import Statistics.Types (Estimator, Sample)

-- | A resample drawn randomly, with replacement, from a set of data
-- points.  Distinct from a normal array to make it harder for your
-- humble author's brain to go wrong.
newtype Resample = Resample {
      fromResample :: U.Vector Double
    } deriving (Eq, Show)

-- | Resample a data set repeatedly, with replacement, computing each
-- estimate over the resampled data.
resample :: (PrimMonad m) => Gen (PrimState m) -> [Estimator] -> Int -> Sample -> m [Resample]
resample gen ests numResamples samples = do
  results <- mapM (const (MU.new numResamples)) $ ests
  loop 0 (zip ests results)
  mapM_ sort results
  mapM (liftM Resample . unsafeFreeze) results
 where
  loop k ers | k >= numResamples = return ()
             | otherwise = do
    re <- create n $ \_ -> do
            r <- uniform gen
            return (samples ! (abs r `mod` n))
    forM_ ers $ \(est,arr) ->
        MU.write arr k . est $ re
    loop (k+1) ers
  n = U.length samples

-- | Compute a statistical estimate repeatedly over a sample, each
-- time omitting a successive element.
jackknife :: Estimator -> Sample -> U.Vector Double
jackknife est sample = U.map f . indices $ sample
    where f i = est (dropAt i sample)
{-# INLINE jackknife #-}

-- | Drop the /k/th element of a vector.
dropAt :: U.Unbox e => Int -> U.Vector e -> U.Vector e
dropAt n = U.map snd . U.filter notN . indexed
    where notN (i , _) = i /= n
