{-# LANGUAGE PatternGuards #-}
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
    , fromRowLists
    , fromRows
    , fromColumns
    , toVector
    , toList
    , toRows
    , toColumns
    , toRowLists
      -- * Other
    , generate
    , generateSym
    , ident
    , diag
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

import Prelude hiding (exponent, map)
import Control.Applicative ((<$>))
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed   ((!))
import qualified Data.Vector.Unboxed.Mutable as UM

import Numeric.Sum         (sumVector,kbn)
import Statistics.Function (for, square)
import Statistics.Matrix.Types
import Statistics.Matrix.Mutable  (unsafeNew,unsafeWrite,unsafeFreeze)



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
fromRowLists :: [[Double]] -> Matrix
fromRowLists = fromRows . fmap U.fromList

-- | Convert from a row-major vector.
fromVector :: Int               -- ^ Number of rows.
           -> Int               -- ^ Number of columns.
           -> U.Vector Double   -- ^ Flat list of values, in row-major order.
           -> Matrix
fromVector r c v
  | r*c /= len = error "input size mismatch"
  | otherwise  = Matrix r c v
  where len    = U.length v

-- | create a matrix from a list of vectors, as rows
fromRows :: [Vector] -> Matrix
fromRows xs
  | [] <- xs        = error "Statistics.Matrix.fromRows: empty list of rows!"
  | any (/=nCol) ns = error "Statistics.Matrix.fromRows: row sizes do not match"
  | nCol == 0       = error "Statistics.Matrix.fromRows: zero columns in matrix"
  | otherwise       = fromVector nRow nCol (U.concat xs)
  where
    nCol:ns = U.length <$> xs
    nRow    = length xs


-- | create a matrix from a list of vectors, as columns
fromColumns :: [Vector] -> Matrix
fromColumns = transpose . fromRows

-- | Convert to a row-major flat vector.
toVector :: Matrix -> U.Vector Double
toVector (Matrix _ _ v) = v

-- | Convert to a row-major flat list.
toList :: Matrix -> [Double]
toList = U.toList . toVector

-- | Convert to a list of lists, as rows
toRowLists :: Matrix -> [[Double]]
toRowLists (Matrix _ nCol v)
  = chunks $ U.toList v
  where
    chunks [] = []
    chunks xs = case splitAt nCol xs of
      (rowE,rest) -> rowE : chunks rest


-- | Convert to a list of vectors, as rows
toRows :: Matrix -> [Vector]
toRows (Matrix _ nCol v) = chunks v
  where
    chunks xs
      | U.null xs = []
      | otherwise = case U.splitAt nCol xs of
          (rowE,rest) -> rowE : chunks rest

-- | Convert to a list of vectors, as columns
toColumns :: Matrix -> [Vector]
toColumns = toRows . transpose



----------------------------------------------------------------
-- Other
----------------------------------------------------------------

-- | Generate matrix using function
generate :: Int                 -- ^ Number of rows
         -> Int                 -- ^ Number of columns
         -> (Int -> Int -> Double)
            -- ^ Function which takes /row/ and /column/ as argument.
         -> Matrix
generate nRow nCol f
  = Matrix nRow nCol $ U.generate (nRow*nCol) $ \i ->
      let (r,c) = i `quotRem` nCol in f r c

-- | Generate symmetric square matrix using function
generateSym
  :: Int                 -- ^ Number of rows and columns
  -> (Int -> Int -> Double)
     -- ^ Function which takes /row/ and /column/ as argument. It must
     --   be symmetric in arguments: @f i j == f j i@
  -> Matrix
generateSym n f = runST $ do
  m <- unsafeNew n n
  for 0 n $ \r -> do
    unsafeWrite m r r (f r r)
    for (r+1) n $ \c -> do
      let x = f r c
      unsafeWrite m r c x
      unsafeWrite m c r x
  unsafeFreeze m


-- | Create the square identity matrix with given dimensions.
ident :: Int -> Matrix
ident n = diag $ U.replicate n 1.0

-- | Create a square matrix with given diagonal, other entries default to 0
diag :: Vector -> Matrix
diag v
  = Matrix n n $ U.create $ do
      arr <- UM.replicate (n*n) 0
      for 0 n $ \i ->
        UM.unsafeWrite arr (i*n + i) (v ! i)
      return arr
  where
    n = U.length v

-- | Return the dimensions of this matrix, as a (row,column) pair.
dimension :: Matrix -> (Int, Int)
dimension (Matrix r c _) = (r, c)

-- | Matrix-matrix multiplication. Matrices must be of compatible
-- sizes (/note: not checked/).
multiply :: Matrix -> Matrix -> Matrix
multiply m1@(Matrix r1 _ _) m2@(Matrix _ c2 _) =
  Matrix r1 c2 $ U.generate (r1*c2) go
  where
    go t = sumVector kbn $ U.zipWith (*) (row m1 i) (column m2 j)
      where (i,j) = t `quotRem` c2

-- | Matrix-vector multiplication.
multiplyV :: Matrix -> Vector -> Vector
multiplyV m v
  | cols m == c = U.generate (rows m) (sumVector kbn . U.zipWith (*) v . row m)
  | otherwise   = error $ "matrix/vector unconformable " ++ show (cols m,c)
  where c = U.length v

-- | Raise matrix to /n/th power. Power must be positive
-- (/note: not checked).
power :: Matrix -> Int -> Matrix
power mat 1 = mat
power mat n = res
  where
    mat2 = power mat (n `quot` 2)
    pow  = multiply mat2 mat2
    res | odd n     = multiply pow mat
        | otherwise = pow

-- | Element in the center of matrix (not corrected for exponent).
center :: Matrix -> Double
center mat@(Matrix r c _) =
    unsafeBounds U.unsafeIndex mat (r `quot` 2) (c `quot` 2)

-- | Calculate the Euclidean norm of a vector.
norm :: Vector -> Double
norm = sqrt . sumVector kbn . U.map square

-- | Return the given column.
column :: Matrix -> Int -> Vector
column (Matrix r c v) i = U.backpermute v $ U.enumFromStepN i c r
{-# INLINE column #-}

-- | Return the given row.
row :: Matrix -> Int -> Vector
row (Matrix _ c v) i = U.slice (c*i) c v

unsafeIndex :: Matrix
            -> Int              -- ^ Row.
            -> Int              -- ^ Column.
            -> Double
unsafeIndex = unsafeBounds U.unsafeIndex

-- | Apply function to every element of matrix
map :: (Double -> Double) -> Matrix -> Matrix
map f (Matrix r c v) = Matrix r c (U.map f v)

-- | Indicate whether any element of the matrix is @NaN@.
hasNaN :: Matrix -> Bool
hasNaN = U.any isNaN . toVector

-- | Given row and column numbers, calculate the offset into the flat
-- row-major vector.
bounds :: (Vector -> Int -> r) -> Matrix -> Int -> Int -> r
bounds k (Matrix rs cs v) r c
  | r < 0 || r >= rs = error "row out of bounds"
  | c < 0 || c >= cs = error "column out of bounds"
  | otherwise        = k v $! r * cs + c
{-# INLINE bounds #-}

-- | Given row and column numbers, calculate the offset into the flat
-- row-major vector, without checking.
unsafeBounds :: (Vector -> Int -> r) -> Matrix -> Int -> Int -> r
unsafeBounds k (Matrix _ cs v) r c = k v $! r * cs + c
{-# INLINE unsafeBounds #-}

transpose :: Matrix -> Matrix
transpose m@(Matrix r0 c0 _) = Matrix c0 r0 . U.generate (r0*c0) $ \i ->
  let (r,c) = i `quotRem` r0
  in unsafeIndex m c r
