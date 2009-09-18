{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Main (main) where

import Data.List
import Data.Int
import Data.Word
import Control.Monad
import Control.Monad.ST
import Data.Time.Clock.POSIX
import Data.Array.Vector
import Statistics.RandomVariate
import System.Random.Mersenne

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

time :: IO a -> IO (Double, a)
time act = do
  s <- getTime
  ret <- act
  e <- ret `seq` getTime
  return (e-s, ret)

count = floor 1e8

type T = Word32

--summ :: [T] -> T
--summ = foldl' (+) 0

loop :: Monad m => Int -> m T -> m T
loop n act = go 0 0
  where
    go !i !s | i >= n = return s
             | otherwise = do
      d <- act
      go (i+1) (s+d)

mwc :: Word32 -> IO T
mwc k = return $! runST go where
  go = do
    gen <- initialize (singletonU k)
    loop count (uniform gen)

mwca :: Word32 -> IO T
mwca k = return $! runST go where
  go = do
    gen <- initialize (singletonU k)
    sumU `fmap` uniformArray gen count

mersenne :: IO T
mersenne = do
  gen <- getStdGen
  loop count (random gen)

main = do
  forM_ [1..3] $ \n -> do
    putStr "mwc: "
    print =<< time (mwc n)
    putStr "mwca: "
    print =<< time (mwca n)
    putStr "mersenne: "
    print =<< time mersenne
