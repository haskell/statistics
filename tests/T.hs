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
import Text.Printf
import System.IO
import MWC as MWC

type T = Word32

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

time :: String -> IO (a,Int) -> IO ()
time desc act = do
  putStr (desc ++ ": ")
  s <- getTime
  (ret,n) <- act
  e <- getTime
  let t = e-s
  hPrintf stdout "%.3g secs (%.3g M/sec)\n" t (fromIntegral n/t/1e6)

count = 100 * 1000000

--summ :: [T] -> T
--summ = foldl' (+) 0

loop :: Monad m => Int -> m T -> m T
loop n act = go 0 0
  where
    go !i !s | i >= n = return s
             | otherwise = do
      d <- act
      go (i+1) (s+d)

system :: IO (Double, Int)
system = do
    gen <- R.getStdGen
    let a :*: g = loop 0 0 gen
    R.setStdGen g
    a `seq` return (a, k)
  where loop !i !a !g | i == k = a :*: g
                      | otherwise = let (b,h) = R.random g
                                    in loop (i+1) (a+b) h
        k = count `div` 100
                 
immut :: Word32 -> IO (Word32, Int)
immut k = let r = loop (MWC.init k) 0 0
          in r `seq` return (r,n)
  where loop !g !i !a | i == n = a
                      | otherwise = let (b:*:h) = MWC.next g
                                    in loop h (i+1) (a+b)
        n = count `div` 10
  

mwc :: Word32 -> IO (T, Int)
mwc k = let r = runST go
        in r `seq` return (r,count)
  where
    go = do
      gen <- initialize (singletonU k)
      loop count (uniform gen)

mwca :: Word32 -> IO (T, Int)
mwca k = let r = runST go
         in r `seq` return (r,count)
  where
    go = do
      gen <- initialize (singletonU k)
      sumU `fmap` uniformArray gen count

mersenne :: IO (T, Int)
mersenne = do
  gen <- M.getStdGen
  r <- loop count (M.random gen)
  return (r,count)

main = do
  forM_ [1..3] $ \n -> do
    time "system" system
    time "immut" (immut n)
    time "mwc" (mwc n)
    time "mwca" (mwca n)
    time "mersenne" mersenne
