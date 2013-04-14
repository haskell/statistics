{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric #-}

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

import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Monad (forM_, liftM, replicateM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Binary (Binary(..))
import Data.Data (Data, Typeable)
import Data.Vector.Algorithms.Intro (sort)
import Data.Vector.Binary ()
import Data.Vector.Generic (unsafeFreeze)
import Data.Word (Word32)
import GHC.Conc (numCapabilities)
import GHC.Generics (Generic)
import Statistics.Function (indices)
import Statistics.Types (Estimator, Sample)
import System.Random.MWC (Gen, initialize, uniform, uniformVector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | A resample drawn randomly, with replacement, from a set of data
-- points.  Distinct from a normal array to make it harder for your
-- humble author's brain to go wrong.
newtype Resample = Resample {
      fromResample :: U.Vector Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary Resample

-- | /O(e*r*s)/ Resample a data set repeatedly, with replacement,
-- computing each estimate over the resampled data.
--
-- This function is expensive; it has to do work proportional to
-- /e*r*s/, where /e/ is the number of estimation functions, /r/ is
-- the number of resamples to compute, and /s/ is the number of
-- original samples.
--
-- To improve performance, this function will make use of all
-- available CPUs.  At least with GHC 7.0, parallel performance seems
-- best if the parallel garbage collector is disabled (RTS option
-- @-qg@).
resample :: Gen (PrimState IO)
         -> [Estimator]         -- ^ Estimation functions.
         -> Int                 -- ^ Number of resamples to compute.
         -> Sample              -- ^ Original sample.
         -> IO [Resample]
resample gen ests numResamples samples = do
  let !numSamples = U.length samples
      ixs = scanl (+) 0 $
            zipWith (+) (replicate numCapabilities q)
                        (replicate r 1 ++ repeat 0)
          where (q,r) = numResamples `quotRem` numCapabilities
  results <- mapM (const (MU.new numResamples)) ests
  done <- newChan
  forM_ (zip ixs (tail ixs)) $ \ (start,!end) -> do
    gen' <- initialize =<< (uniformVector gen 256 :: IO (U.Vector Word32))
    forkIO $ do
      let loop k ers | k >= end = writeChan done ()
                     | otherwise = do
            re <- U.replicateM numSamples $ do
                    r <- uniform gen'
                    return (U.unsafeIndex samples (r `mod` numSamples))
            forM_ ers $ \(est,arr) ->
                MU.write arr k . est $ re
            loop (k+1) ers
      loop start (zip ests results)
  replicateM_ numCapabilities $ readChan done
  mapM_ sort results
  mapM (liftM Resample . unsafeFreeze) results

-- | Compute a statistical estimate repeatedly over a sample, each
-- time omitting a successive element.
jackknife :: Estimator -> Sample -> U.Vector Double
jackknife est sample = U.map f . indices $ sample
    where f i = est (dropAt i sample)
{- INLINE jackknife #-}

-- | Drop the /k/th element of a vector.
dropAt :: U.Unbox e => Int -> U.Vector e -> U.Vector e
dropAt n v = U.slice 0 n v U.++ U.slice (n+1) (U.length v - n - 1) v
