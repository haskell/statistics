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
  e <- getTime
  return (e-s, ret)

count = 50000000

type T = Word64

--summ :: [T] -> T
--summ = foldl' (+) 0

loop :: Monad m => T -> m T -> m T
loop n act = go 0 0
  where
    go !i !s | i >= n = return s
             | otherwise = do
      d <- act
      go (i+1) (s+fromIntegral d)

mwc :: Word32 -> IO T
mwc k = return $! runST go where
  go = do
    gen <- initialize (singletonU k)
    loop count (uniform gen)

mersenne :: IO T
mersenne = do
  gen <- getStdGen
  loop count (random gen)

main = do
  forM_ [1..3] $ \n -> do
    putStr "mwc: "
    print =<< time (mwc n)
    putStr "mersenne: "
    print =<< time mersenne
