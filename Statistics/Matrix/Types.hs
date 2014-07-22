-- |
-- Module    : Statistics.Matrix.Types
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Basic matrix operations.
--
-- There isn't a widely used matrix package for Haskell yet, so
-- we implement the necessary minimum here.

module Statistics.Matrix.Types
    (
      Vector
    , MVector
    , Matrix(..)
    , MMatrix(..)
    , debug
    ) where

import Data.Char (isSpace)
import Numeric (showFFloat)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

type Vector = U.Vector Double
type MVector s = M.MVector s Double

-- | Two-dimensional matrix, stored in row-major order.
data Matrix = Matrix {
      rows     :: {-# UNPACK #-} !Int -- ^ Rows of matrix.
    , cols     :: {-# UNPACK #-} !Int -- ^ Columns of matrix.
    , exponent :: {-# UNPACK #-} !Int
      -- ^ In order to avoid overflows during matrix multiplication, a
      -- large exponent is stored separately.
    , _vector  :: !Vector  -- ^ Matrix data.
    } deriving (Eq)

-- | Two-dimensional mutable matrix, stored in row-major order.
data MMatrix s = MMatrix
                 {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Int
                 !(MVector s)

-- The Show instance is useful only for debugging.
instance Show Matrix where
    show = debug

debug :: Matrix -> String
debug (Matrix r c _ vs) = unlines $ zipWith (++) (hdr0 : repeat hdr) rrows
  where
    rrows         = map (cleanEnd . unwords) . split $ zipWith (++) ldone tdone
    hdr0          = show (r,c) ++ " "
    hdr           = replicate (length hdr0) ' '
    pad plus k xs = replicate (k - length xs) ' ' `plus` xs
    ldone         = map (pad (++) (longest lstr)) lstr
    tdone         = map (pad (flip (++)) (longest tstr)) tstr
    (lstr, tstr)  = unzip . map (break (=='.') . render) . U.toList $ vs
    longest       = maximum . map length
    render k      = reverse . dropWhile (=='.') . dropWhile (=='0') . reverse .
                    showFFloat (Just 4) k $ ""
    split []      = []
    split xs      = i : split rest where (i, rest) = splitAt c xs
    cleanEnd      = reverse . dropWhile isSpace . reverse
