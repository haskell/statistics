-- |
-- Module    : Statistics.Regression
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Functions for regression analysis.

module Statistics.Regression
    (
      ols
    , rSquare
    ) where

import Control.Applicative ((<$>))
import Prelude hiding (sum)
import Statistics.Function as F
import Statistics.Matrix
import Statistics.Matrix.Algorithms (qr)
import Statistics.Sample (mean)
import Statistics.Sample.Internal (sum)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

-- | Compute the ordinary least-squares solution to /A x = b/.
ols :: Matrix     -- ^ /A/ has at least as many rows as columns.
    -> Vector     -- ^ /b/ has the same length as columns in /A/.
    -> Vector
ols a b
  | rs < cs   = error $ "fewer rows than columns " ++ show d
  | otherwise = solve r (transpose q `multiplyV` b)
  where
    d@(rs,cs) = dimension a
    (q,r)     = qr a

-- | Solve the equation /R x = b/.
solve :: Matrix     -- ^ /R/ is an upper-triangular square matrix.
      -> Vector     -- ^ /b/ is of the same length as rows\/columns in /R/.
      -> Vector
solve r b
  | n /= l    = error $ "row/vector mismatch " ++ show (n,l)
  | otherwise = U.create $ do
  s <- U.thaw b
  rfor n 0 $ \i -> do
    si <- (/ unsafeIndex r i i) <$> M.unsafeRead s i
    M.unsafeWrite s i si
    for 0 i $ \j -> F.unsafeModify s j $ subtract ((unsafeIndex r j i) * si)
  return s
  where n = rows r
        l = U.length b

-- | Compute /R&#0178;/, the coefficient of determination that
-- indicates goodness-of-fit of a regression.
--
-- This value will be 1 if the predictors fit perfectly, dropping to 0
-- if they have no explanatory power.
rSquare :: Matrix               -- ^ Predictors (regressors).
        -> Vector               -- ^ Responders.
        -> Vector               -- ^ Regression coefficients.
        -> Double
rSquare pred resp coeff = 1 - r / t
  where
    r   = sum $ flip U.imap resp $ \i x -> square (x - p i)
    t   = sum $ flip U.map resp $ \x -> square (x - mean resp)
    p i = sum . flip U.imap coeff $ \j -> (* unsafeIndex pred i j)
