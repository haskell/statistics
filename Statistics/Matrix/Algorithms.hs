-- |
-- Module    : Statistics.Matrix.Algorithms
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Useful matrix functions.

{-# LANGUAGE BangPatterns #-}

module Statistics.Matrix.Algorithms
    (
      qr
    , khun_munkres_min
    , khun_munkres_max
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Cont
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Bits
import Prelude hiding (sum, replicate)
import Statistics.Function ( forStep,unsafeModify,setbit,getbit
                           , getbit',newMBitVec,MBitVec,BitVec)
import Statistics.Matrix ( Matrix, Vector, row, column, dimension
                         , for, norm, rows, cols, _vector)
import qualified Statistics.Matrix.Mutable as M
import Statistics.Sample.Internal (sum)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

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

innerProduct :: M.MMatrix s -> Int -> Int -> ST s Double
innerProduct mmat j k = M.immutably mmat $ \mat ->
  sum $ U.zipWith (*) (column mat j) (column mat k)

---- Khun-Munkres Algorithm ----

-- | /O(n^3)/ Compute the maximal assignment of tasks
-- from the given cost matrix using the Khun-Munrkes algorithm.
-- Requirements: square matrix, non-negative entries
khun_munkres_max :: Matrix -> U.Vector Int
khun_munkres_max mat = khun_munkres_min mat'
    where mat' = let v = _vector mat
                     x = U.maximum v
                 in mat{_vector = U.map (x-) v}

-- | /O(n^3)/ Compute the minimal assignment of tasks
-- from the given cost matrix using the Khun-Munrkes algorithm.
-- Requirements: square matrix, non-negative entries
khun_munkres_min :: Matrix -> U.Vector Int
khun_munkres_min mat
    | rows mat /= cols mat = error "khun_munkres: not a square matrix!"
    | U.any (<0) (_vector mat) = error "khun_munkres: matrix has non-negative entries!"
    | otherwise = runST $ do
        let dim = rows mat
        m@(M.MMatrix _ _ _ mv) <- M.thaw mat

-- subtract min from each row and column
        for 0 dim $ \r -> let x = U.minimum (row mat r)
                          in for (r*dim) (r*dim + dim) (\i -> unsafeModify mv i (subtract x))
        mat' <- M.unsafeFreeze m
        for 0 dim $ \c -> let x = U.minimum (column mat' c)
                          in forStep c (dim*dim + c) dim (\i -> unsafeModify mv i (subtract x))
        khun_iterate m

-- Find the assignment
khun_iterate :: M.MMatrix s -> ST s (U.Vector Int)
khun_iterate mat@(M.MMatrix dim _ _ _) = do
  marked_cols <- newMBitVec dim
  asgn <- MU.new (dim+1)
  let loop = do
        MU.set marked_cols 0
        MU.set asgn (-1)
        MU.unsafeWrite asgn dim 0
        khun_selections mat asgn marked_cols 0
        num <- MU.unsafeRead asgn dim
        assigns <- U.unsafeFreeze asgn >>= return . U.init
        if num == dim
         then return assigns
         else khun_cover mat assigns >>= uncurry (khun_createZero mat) >>
              loop
  loop

-- Cover the zeros with the minimum number of lines given a maximal
-- assignment.
khun_cover :: M.MMatrix s -> U.Vector Int -> ST s (BitVec,BitVec)
khun_cover m av = do
  mat <- M.unsafeFreeze m
  br <- newMBitVec (rows mat)
  bc <- newMBitVec (rows mat)
  let checkRow r = getbit br r >>= \b -> when (not b) $ do
                     setbit br r
                     U.mapM_ (\(c,x) -> when (x==0) (checkCol c))
                             (U.indexed $ row mat r)
      checkCol c = getbit bc c >>= \b -> when (not b) $ do
                     setbit bc c
                     U.mapM_ (\(r,x) -> when (x== -1) (checkRow r))
                             (U.indexed $ column mat c)
  U.mapM_ (\(r,c) -> when (c== -1) $ checkRow r) (U.indexed av)
  U.mapM_ (\(r,c) -> when (c/= -1) $ M.unsafeWrite m r c 0) (U.indexed av)
  brows <- U.unsafeFreeze br >>= return . U.map complement
  bcols <- U.unsafeFreeze bc
  return (brows,bcols)

-- Create zero given the cover
khun_createZero :: M.MMatrix s -> BitVec -> BitVec -> ST s ()
khun_createZero (M.MMatrix dim _ _ m) brows bcols = do
  !x <- U.unsafeFreeze m >>= return . khun_pickMin dim brows bcols

-- Subtract x from uncovered rows

  mapM_ (\r -> when (not $ getbit' brows r) $
                 forStep (dim*r) (dim*r + dim) 1 (\i -> unsafeModify m i (subtract x))
        ) [0..dim-1]

-- Add x to covered columns

  mapM_ (\c -> when (getbit' bcols c) $
                 forStep c (dim*dim + c) dim (\i -> unsafeModify m i (+x))
        ) [0..dim-1]

-- Pick smallest value from uncovered cells
khun_pickMin :: Int -> BitVec -> BitVec -> Vector -> Double
khun_pickMin dim brows bcols m = loop 0 0 (1e400)
    where loop !r !c !acc | r == dim = acc
                          | c == 0 && getbit' brows r = loop (r+1) 0 acc
                          | getbit' bcols c = loop r (c+1) acc
                          | c == dim = loop (r+1) 0 acc
                          | otherwise = loop r (c+1) (acc `min` U.unsafeIndex m (r*dim + c))

khun_selections :: M.MMatrix s -> MU.STVector s Int -> MBitVec s -> Int -> ST s ()
khun_selections mmat@(M.MMatrix dim _ _ _) asgn taken_cols r
    | r == dim = return ()
    | otherwise = do
        rowvec <- M.unsafeFreeze mmat >>= return . flip row r
        maybec <- runMaybeT . msum . flip Prelude.map (U.toList $ U.indexed rowvec) $ \(c,x) -> do
                    guard (x==0)
                    c_taken <- lift $ getbit taken_cols c
                    guard (not c_taken)
                    return c
        case maybec of
          Nothing -> khun_try_select mmat asgn taken_cols r
          Just c  -> MU.unsafeRead asgn dim >>=
                     MU.unsafeWrite asgn dim . (+1) >>
                     MU.unsafeWrite asgn r c >>
                     setbit taken_cols c >>
                     M.unsafeWrite mmat r c (-1)
        khun_selections mmat asgn taken_cols (r+1)

khun_try_select :: M.MMatrix s -> MU.STVector s Int -> MBitVec s -> Int -> ST s ()
khun_try_select mmat@(M.MMatrix dim _ _ _) asgn taken_cols end = void . runMaybeT $ do
  mat <- lift $ M.unsafeFreeze mmat
  visited <- lift $ newMBitVec (end+1)
  let loop foundOne parent_col =
          msum . flip Prelude.map (U.toList . U.take (end+1) . U.indexed $ column mat parent_col) $ \(r,x) -> do
            guard (x == 0)
            r_visited <- lift $ getbit visited r
            guard (not r_visited)
            lift $ setbit visited r
            let (Just r_col) = U.findIndex (== -1) (row mat r)
                mark = lift $ setbit taken_cols parent_col >>
                              MU.unsafeWrite asgn r parent_col >>
                              M.unsafeWrite mmat r parent_col (-1)
            if r == end
              then guard foundOne >>
                   lift (MU.unsafeRead asgn dim >>= MU.unsafeWrite asgn dim . (+1)) >>
                   mark >> return ()
              else loop True r_col >>= \_ -> mark >>
                   lift (M.unsafeWrite mmat r r_col 0)

  msum $ Prelude.map (\c -> do
                        c_taken <- lift $ getbit taken_cols c
                        guard (not c_taken)
                        loop False c
                     ) [0..dim-1]
