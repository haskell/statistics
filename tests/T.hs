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
import qualified System.Random as R
import qualified System.Random.Mersenne as M

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

time :: IO a -> IO (Double, a)
time act = do
  s <- getTime
  ret <- act
  e <- ret `seq` getTime
  return (e-s, ret)

count = 100 * 1000000

type T = Double

--summ :: [T] -> T
--summ = foldl' (+) 0

loop :: Monad m => Int -> m T -> m T
loop n act = go 0 0
  where
    go !i !s | i >= n = return s
             | otherwise = do
      d <- act
      go (i+1) (s+d)

system :: IO Double
system = do
    gen <- R.getStdGen
    let a :*: g = loop 0 0 gen
    R.setStdGen g
    return a
  where loop !i !a !g | i == k = a :*: g
                      | otherwise = let (b,h) = R.random g
                                    in loop (i+1) (a+b) h
        k = count `div` 100
                 

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
  gen <- M.getStdGen
  loop count (M.random gen)

main = do
  forM_ [1..3] $ \n -> do
    putStr "system: "
    print =<< time system
    putStr "mwc: "
    print =<< time (mwc n)
    putStr "mwca: "
    print =<< time (mwca n)
    putStr "mersenne: "
    print =<< time mersenne
