-- |
-- Module    : Statistics.Matrix.Algorithms
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Useful matrix functions.

module Statistics.Matrix.Algorithms
    (
      qr
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.ST (ST, runST)
import Control.Monad (when)
import Prelude hiding (sum, replicate)
import Statistics.Matrix (Matrix, column, dimension, for, norm)
import qualified Statistics.Matrix.Mutable as M
import Statistics.Sample.Internal (sum)
import qualified Data.Vector.Unboxed as U

-- | /O(r*c)/ Compute the QR decomposition of a matrix.
-- The result returned is the matrices (/q/,/r/).
qr :: Matrix -> (Matrix, Matrix)
qr mat = runST $ do
  let (m,n) = dimension mat
  r <- M.replicate n n 0
  a <- M.thaw mat
  for 0 n $ \j -> do
    cn <- M.immutably a $ \aa -> norm (column aa j)
    M.unsafeWrite r j j cn
    for 0 m $ \i -> M.unsafeModify a i j (/ cn)
    for (j+1) n $ \jj -> do
      p <- innerProduct a j jj
      M.unsafeWrite r j jj p
      for 0 m $ \i -> do
        aij <- M.unsafeRead a i j
        M.unsafeModify a i jj $ subtract (p * aij)
  (,) <$> M.unsafeFreeze a <*> M.unsafeFreeze r

-- | /O(n^3)/ Compute the Cholesky factorization and return
-- the lower triangular Cholesky factor (/L/). Note: does
-- not check whether matrix is positive definite or not.
-- Will fail if diagonal elements are non-positive.  
chol :: Matrix -> Matrix
chol mat
  | m /= n    = error "Matrix must be square."
  | otherwise = runST $ do
      l <- M.thaw mat
      for 0 n $ \j -> do
        M.unsafeModify l j j sqrt
        for (j+1) n $ \i -> do
          ljj <- M.unsafeRead l j j
          M.unsafeModify l i j  (/ ljj)
          M.unsafeWrite l j i 0
          for (j+1) (i+1) $ \jj -> do
            ljjj <- M.unsafeRead l jj j
            lij  <- M.unsafeRead l i j
            M.unsafeModify l i jj (subtract $ lij*ljjj)
            when (i /= jj) $
              M.unsafeModify l jj i (subtract $ lij*ljjj)
      M.unsafeFreeze l
  where (m,n) = dimension mat

innerProduct :: M.MMatrix s -> Int -> Int -> ST s Double
innerProduct mmat j k = M.immutably mmat $ \mat ->
  sum $ U.zipWith (*) (column mat j) (column mat k)
