-- |
-- Module    : Statistics.Matrix
-- Copyright : (c) 2011 Aleksey Khudyakov
-- License   : BSD3
--
-- Basic matrix operations.
--
-- There isn't a widely used matrix package for Haskell yet, so
-- we implement the necessary minimum here.

module Statistics.Matrix
    (
      Matrix(..)
    , center
    , multiply
    , power
    ) where

import Prelude hiding (sum)
import Statistics.Sample.Internal (sum)
import Text.Printf (printf)
import qualified Data.Vector.Unboxed as U

----------------------------------------------------------------


-- | Square matrix stored in row-major order.
data Matrix = Matrix
              {-# UNPACK #-} !Int -- ^ Size of matrix.
              !(U.Vector Double)  -- ^ Matrix data.
              {-# UNPACK #-} !Int
              -- ^ In order to avoid overflows during matrix
              -- multiplication, a large exponent is stored
              -- separately.

-- The Show instance is useful mostly for debugging.
instance Show Matrix where
  show (Matrix n vs _) = unlines . map (unwords . map (printf "%.4f")) .
                         split . U.toList $ vs
    where
      split [] = []
      split xs = row : split rest where (row, rest) = splitAt n xs


-- | Avoid overflow in the matrix.
avoidOverflow :: Matrix -> Matrix
avoidOverflow m@(Matrix n xs e)
  | center m > 1e140 = Matrix n (U.map (* 1e-140) xs) (e + 140)
  | otherwise        = m

-- | Matrix-matrix multiplication. Matrices must be of the same
-- size (/note: not checked/).
multiply :: Matrix -> Matrix -> Matrix
multiply (Matrix n xs e1) (Matrix _ ys e2) =
  Matrix n (U.generate (n*n) go) (e1 + e2)
  where
    go i = sum $ U.zipWith (*) row col
      where
        nCol = i `rem` n
        row  = U.slice (i - nCol) n xs
        col  = U.backpermute ys $ U.enumFromStepN nCol n n

-- | Raise matrix to /n/th power. Power must be positive
-- (/note: not checked).
power :: Matrix -> Int -> Matrix
power mat 1 = mat
power mat n = avoidOverflow res
  where
    mat2 = power mat (n `quot` 2)
    pow  = multiply mat2 mat2
    res | odd n     = multiply pow mat
        | otherwise = pow

-- | Element in the center of matrix (not corrected for exponent).
center :: Matrix -> Double
center (Matrix n xs _) = (U.!) xs (k*n + k) where k = n `quot` 2
