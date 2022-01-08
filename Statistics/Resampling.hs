{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

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
    ( -- * Data types
      Resample(..)
    , Bootstrap(..)
    , Estimator(..)
    , estimate
      -- * Resampling
    , resampleST
    , resample
    , resampleVector
      -- * Jackknife
    , jackknife
    , jackknifeMean
    , jackknifeVariance
    , jackknifeVarianceUnb
    , jackknifeStdDev
      -- * Helper functions
    , splitGen
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_, forM, replicateM, liftM2)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Binary (Binary(..))
import Data.Data (Data, Typeable)
import Data.Vector.Algorithms.Intro (sort)
import Data.Vector.Binary ()
import Data.Vector.Generic (unsafeFreeze,unsafeThaw)
import Data.Word (Word32)
import qualified Data.Foldable as T
import qualified Data.Traversable as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import GHC.Conc (numCapabilities)
import GHC.Generics (Generic)
import Numeric.Sum (Summation(..), kbn)
import Statistics.Function (indices)
import Statistics.Sample (mean, stdDev, variance, varianceUnbiased)
import Statistics.Types (Sample)
import System.Random.MWC (Gen, GenIO, initialize, uniformR, uniformVector)


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | A resample drawn randomly, with replacement, from a set of data
-- points.  Distinct from a normal array to make it harder for your
-- humble author's brain to go wrong.
newtype Resample = Resample {
      fromResample :: U.Vector Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON Resample
instance ToJSON Resample

instance Binary Resample where
    put = put . fromResample
    get = fmap Resample get

data Bootstrap v a = Bootstrap
  { fullSample :: !a
  , resamples  :: v a
  }
  deriving (Eq, Read, Show , Generic, Functor, T.Foldable, T.Traversable
           , Typeable, Data
           )

instance (Binary a,   Binary   (v a)) => Binary   (Bootstrap v a) where
  get = liftM2 Bootstrap get get
  put (Bootstrap fs rs) = put fs >> put rs
instance (FromJSON a, FromJSON (v a)) => FromJSON (Bootstrap v a)
instance (ToJSON a,   ToJSON   (v a)) => ToJSON   (Bootstrap v a)



-- | An estimator of a property of a sample, such as its 'mean'.
--
-- The use of an algebraic data type here allows functions such as
-- 'jackknife' and 'bootstrapBCA' to use more efficient algorithms
-- when possible.
data Estimator = Mean
               | Variance
               | VarianceUnbiased
               | StdDev
               | Function (Sample -> Double)

-- | Run an 'Estimator' over a sample.
estimate :: Estimator -> Sample -> Double
estimate Mean             = mean
estimate Variance         = variance
estimate VarianceUnbiased = varianceUnbiased
estimate StdDev           = stdDev
estimate (Function est) = est


----------------------------------------------------------------
-- Resampling
----------------------------------------------------------------

-- | Single threaded and deterministic version of resample.
resampleST :: PrimMonad m
           => Gen (PrimState m)
           -> [Estimator]         -- ^ Estimation functions.
           -> Int                 -- ^ Number of resamples to compute.
           -> U.Vector Double     -- ^ Original sample.
           -> m [Bootstrap U.Vector Double]
resampleST gen ests numResamples sample = do
  -- Generate resamples
  res <- forM ests $ \e -> U.replicateM numResamples $ do
    v <- resampleVector gen sample
    return $! estimate e v
  -- Sort resamples
  resM <- mapM unsafeThaw res
  mapM_ sort resM
  resSorted <- mapM unsafeFreeze resM
  return $ zipWith Bootstrap [estimate e sample | e <- ests]
                             resSorted


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
resample :: GenIO
         -> [Estimator]         -- ^ Estimation functions.
         -> Int                 -- ^ Number of resamples to compute.
         -> U.Vector Double     -- ^ Original sample.
         -> IO [(Estimator, Bootstrap U.Vector Double)]
resample gen ests numResamples samples = do
  let ixs = scanl (+) 0 $
            zipWith (+) (replicate numCapabilities q)
                        (replicate r 1 ++ repeat 0)
          where (q,r) = numResamples `quotRem` numCapabilities
  results <- mapM (const (MU.new numResamples)) ests
  gens <- splitGen numCapabilities gen
  forConcurrently_ (zip3 ixs (tail ixs) gens) $ \ (start,!end,gen') -> do
    -- on GHCJS it doesn't make sense to do any forking.
    -- JavaScript runtime has only single capability.
      let loop k ers | k >= end = return ()
                     | otherwise = do
            re <- resampleVector gen' samples
            forM_ ers $ \(est,arr) ->
                MU.write arr k . est $ re
            loop (k+1) ers
      loop start (zip ests' results)
  mapM_ sort results
  -- Build resamples
  res <- mapM unsafeFreeze results
  return $ zip ests
         $ zipWith Bootstrap [estimate e samples | e <- ests]
                             res
 where
  ests' = map estimate ests

-- | Create vector using resamples
resampleVector :: (PrimMonad m, G.Vector v a)
               => Gen (PrimState m) -> v a -> m (v a)
resampleVector gen v
  = G.replicateM n $ do i <- uniformR (0,n-1) gen
                        return $! G.unsafeIndex v i
  where
    n = G.length v


----------------------------------------------------------------
-- Jackknife
----------------------------------------------------------------

-- | /O(n) or O(n^2)/ Compute a statistical estimate repeatedly over a
-- sample, each time omitting a successive element.
jackknife :: Estimator -> Sample -> U.Vector Double
jackknife Mean sample             = jackknifeMean sample
jackknife Variance sample         = jackknifeVariance sample
jackknife VarianceUnbiased sample = jackknifeVarianceUnb sample
jackknife StdDev sample = jackknifeStdDev sample
jackknife (Function est) sample
  | G.length sample == 1 = singletonErr "jackknife"
  | otherwise            = U.map f . indices $ sample
  where f i = est (dropAt i sample)

-- | /O(n)/ Compute the jackknife mean of a sample.
jackknifeMean :: Sample -> U.Vector Double
jackknifeMean samp
  | len == 1  = singletonErr "jackknifeMean"
  | otherwise = G.map (/l) $ G.zipWith (+) (pfxSumL samp) (pfxSumR samp)
  where
    l   = fromIntegral (len - 1)
    len = G.length samp

-- | /O(n)/ Compute the jackknife variance of a sample with a
-- correction factor @c@, so we can get either the regular or
-- \"unbiased\" variance.
jackknifeVariance_ :: Double -> Sample -> U.Vector Double
jackknifeVariance_ c samp
  | len == 1  = singletonErr "jackknifeVariance"
  | otherwise = G.zipWith4 go als ars bls brs
  where
    als = pfxSumL . G.map goa $ samp
    ars = pfxSumR . G.map goa $ samp
    goa x = v * v where v = x - m
    bls = pfxSumL . G.map (subtract m) $ samp
    brs = pfxSumR . G.map (subtract m) $ samp
    m = mean samp
    n = fromIntegral len
    go al ar bl br = (al + ar - (b * b) / q) / (q - c)
      where b = bl + br
            q = n - 1
    len = G.length samp

-- | /O(n)/ Compute the unbiased jackknife variance of a sample.
jackknifeVarianceUnb :: Sample -> U.Vector Double
jackknifeVarianceUnb samp
  | G.length samp == 2  = singletonErr "jackknifeVariance"
  | otherwise           = jackknifeVariance_ 1 samp

-- | /O(n)/ Compute the jackknife variance of a sample.
jackknifeVariance :: Sample -> U.Vector Double
jackknifeVariance = jackknifeVariance_ 0

-- | /O(n)/ Compute the jackknife standard deviation of a sample.
jackknifeStdDev :: Sample -> U.Vector Double
jackknifeStdDev = G.map sqrt . jackknifeVarianceUnb

pfxSumL :: U.Vector Double -> U.Vector Double
pfxSumL = G.map kbn . G.scanl add zero

pfxSumR :: U.Vector Double -> U.Vector Double
pfxSumR = G.tail . G.map kbn . G.scanr (flip add) zero

-- | Drop the /k/th element of a vector.
dropAt :: U.Unbox e => Int -> U.Vector e -> U.Vector e
dropAt n v = U.slice 0 n v U.++ U.slice (n+1) (U.length v - n - 1) v

singletonErr :: String -> a
singletonErr func = error $
                    "Statistics.Resampling." ++ func ++ ": not enough elements in sample"

-- | Split a generator into several that can run independently.
splitGen :: Int -> GenIO -> IO [GenIO]
splitGen n gen
  | n <= 0    = return []
  | otherwise =
  fmap (gen:) . replicateM (n-1) $
  initialize =<< (uniformVector gen 256 :: IO (U.Vector Word32))
