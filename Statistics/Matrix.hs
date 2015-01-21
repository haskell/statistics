-- |
-- Module    : Statistics.Matrix
-- Copyright : 2011 Aleksey Khudyakov, 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Basic matrix operations.
--
-- There isn't a widely used matrix package for Haskell yet, so
-- we implement the necessary minimum here.

module Statistics.Matrix
    ( -- * Data types
      Matrix(..)
    , Vector
      -- * Conversion from/to lists/vectors
    , fromVector
    , fromList
    , fromLists
    , fromRows
    , fromColumns
      -- * Other
    , ident
    , diag
    , diagRect
    , toVector
    , toList
    , toRows
    , toColumns
    , toLists
    , dimension
    , center
    , multiply
    , multiplyV
    , transpose
    , power
    , norm
    , column
    , row
    , map
    , for
    , unsafeIndex
    , hasNaN
    , bounds
    , unsafeBounds
    ) where

import Prelude hiding (exponent, map, sum)
import Statistics.Function (for, square)
import Statistics.Matrix.Types
import Statistics.Sample.Internal (sum)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM


----------------------------------------------------------------
-- Conversion to/from vectors/lists
----------------------------------------------------------------

-- | Convert from a row-major list.
fromList :: Int                 -- ^ Number of rows.
         -> Int                 -- ^ Number of columns.
         -> [Double]            -- ^ Flat list of values, in row-major order.
         -> Matrix
fromList r c = fromVector r c . U.fromList

-- | create a matrix from a list of lists, as rows
fromLists :: [[Double]] -> Matrix
-- FIXME: doesn't check that matrix is correct
fromLists xs = fromVector nrow ncol . U.fromList . concat $ xs
  where
    nrow = length xs
    ncol = length . head $ xs

-- | Convert from a row-major vector.
fromVector :: Int               -- ^ Number of rows.
           -> Int               -- ^ Number of columns.
           -> U.Vector Double   -- ^ Flat list of values, in row-major order.
           -> Matrix
fromVector r c v
  | r*c /= len = error "input size mismatch"
  | otherwise  = Matrix r c 0 v
  where len    = U.length v

-- | create a matrix from a list of vectors, as rows
fromRows :: [Vector] -> Matrix
-- FIXME: doesn't check that matrix is correct
fromRows xs = fromVector r c . U.concat $ xs
  where
    r = length xs
    c = U.length . head $ xs

-- | create a matrix from a list of vectors, as columns
fromColumns :: [Vector] -> Matrix
fromColumns = transpose . fromRows

-- | Convert to a row-major flat vector.
toVector :: Matrix -> U.Vector Double
toVector (Matrix _ _ _ v) = v

-- | Convert to a row-major flat list.
toList :: Matrix -> [Double]
toList = U.toList . toVector

-- | Convert to a list of lists, as rows
toLists :: Matrix -> [[Double]]
toLists (Matrix _ c _ v) = chunksOf' c . U.toList $ v
  where
    chunksOf' _ [] = []
    chunksOf' i xs = take i xs : chunksOf' i (drop i xs)

-- | Convert to a list of vectors, as rows
toRows :: Matrix -> [Vector]
toRows (Matrix _ c _ v) = chunksOf' c v
  where
    chunksOf' i xs | U.length xs > 0 = U.take i xs : chunksOf' i (U.drop i xs)
                   | otherwise = []

-- | Convert to a list of vectors, as columns
toColumns :: Matrix -> [Vector]
toColumns = toRows . transpose



----------------------------------------------------------------
-- Other
----------------------------------------------------------------

-- | Create the square identity matrix with given dimensions.
ident :: Int -> Matrix
ident n = diagRect 0 n n $ U.replicate n 1.0

-- | create a square matrix with given diagonal, other entries default to 0
diag :: Vector -> Matrix
diag v = diagRect 0 n n v
  where n = U.length v

-- | creates a rectangular matrix with default values and given diagonal 
diagRect :: Double              -- ^ Default value
         -> Int                 -- ^ Number of rows
         -> Int                 -- ^ Number of columns
         -> Vector              -- ^ Diagonal
         -> Matrix
diagRect z0 r c d = fromVector r c $ U.create $ UM.replicate n z0 >>= loop 0
  where
    loop i v | i >= l = return v
             | otherwise = UM.write v (i*(c+1)) (d U.! i) >> loop (i+1) v
    l = U.length d
    n = r * c
{-# INLINE diagRect #-}

-- | Return the dimensions of this matrix, as a (row,column) pair.
dimension :: Matrix -> (Int, Int)
dimension (Matrix r c _ _) = (r, c)

-- | Avoid overflow in the matrix.
avoidOverflow :: Matrix -> Matrix
avoidOverflow m@(Matrix r c e v)
  | center m > 1e140 = Matrix r c (e + 140) (U.map (* 1e-140) v)
  | otherwise        = m

-- | Matrix-matrix multiplication. Matrices must be of compatible
-- sizes (/note: not checked/).
multiply :: Matrix -> Matrix -> Matrix
multiply m1@(Matrix r1 _ e1 _) m2@(Matrix _ c2 e2 _) =
  Matrix r1 c2 (e1 + e2) $ U.generate (r1*c2) go
  where
    go t = sum $ U.zipWith (*) (row m1 i) (column m2 j)
      where (i,j) = t `quotRem` c2

-- | Matrix-vector multiplication.
multiplyV :: Matrix -> Vector -> Vector
multiplyV m v
  | cols m == c = U.generate (rows m) (sum . U.zipWith (*) v . row m)
  | otherwise   = error $ "matrix/vector unconformable " ++ show (cols m,c)
  where c = U.length v

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
center mat@(Matrix r c _ _) =
    unsafeBounds U.unsafeIndex mat (r `quot` 2) (c `quot` 2)

-- | Calculate the Euclidean norm of a vector.
norm :: Vector -> Double
norm = sqrt . sum . U.map square

-- | Return the given column.
column :: Matrix -> Int -> Vector
column (Matrix r c _ v) i = U.backpermute v $ U.enumFromStepN i c r
{-# INLINE column #-}

-- | Return the given row.
row :: Matrix -> Int -> Vector
row (Matrix _ c _ v) i = U.slice (c*i) c v

unsafeIndex :: Matrix
            -> Int              -- ^ Row.
            -> Int              -- ^ Column.
            -> Double
unsafeIndex = unsafeBounds U.unsafeIndex

-- | Apply function to every element of matrix
map :: (Double -> Double) -> Matrix -> Matrix
map f (Matrix r c e v) = Matrix r c e (U.map f v)

-- | Indicate whether any element of the matrix is @NaN@.
hasNaN :: Matrix -> Bool
hasNaN = U.any isNaN . toVector

-- | Given row and column numbers, calculate the offset into the flat
-- row-major vector.
bounds :: (Vector -> Int -> r) -> Matrix -> Int -> Int -> r
bounds k (Matrix rs cs _ v) r c
  | r < 0 || r >= rs = error "row out of bounds"
  | c < 0 || c >= cs = error "column out of bounds"
  | otherwise        = k v $! r * cs + c
{-# INLINE bounds #-}

-- | Given row and column numbers, calculate the offset into the flat
-- row-major vector, without checking.
unsafeBounds :: (Vector -> Int -> r) -> Matrix -> Int -> Int -> r
unsafeBounds k (Matrix _ cs _ v) r c = k v $! r * cs + c
{-# INLINE unsafeBounds #-}

transpose :: Matrix -> Matrix
transpose m@(Matrix r0 c0 e _) = Matrix c0 r0 e . U.generate (r0*c0) $ \i ->
  let (r,c) = i `quotRem` r0
  in unsafeIndex m c r
