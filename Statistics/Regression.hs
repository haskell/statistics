-- |
-- Module    : Statistics.Regression
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Functions for regression analysis.

module Statistics.Regression
    (
      olsRegress
    , ols
    , rSquare
    , bootstrapRegress
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.DeepSeq (rnf)
import Control.Monad (forM_, replicateM)
import GHC.Conc (getNumCapabilities)
import Prelude hiding (pred, sum)
import Statistics.Function as F
import Statistics.Matrix hiding (map)
import Statistics.Matrix.Algorithms (qr)
import Statistics.Resampling (splitGen)
import Statistics.Types      (Estimate(..),ConfInt,CL,estimateFromInterval,significanceLevel)
import Statistics.Sample (mean)
import Statistics.Sample.Internal (sum)
import System.Random.MWC (GenIO, uniformR)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

-- | Perform an ordinary least-squares regression on a set of
-- predictors, and calculate the goodness-of-fit of the regression.
--
-- The returned pair consists of:
--
-- * A vector of regression coefficients.  This vector has /one more/
--   element than the list of predictors; the last element is the
--   /y/-intercept value.
--
-- * /R&#0178;/, the coefficient of determination (see 'rSquare' for
--   details).
olsRegress :: [Vector]
              -- ^ Non-empty list of predictor vectors.  Must all have
              -- the same length.  These will become the columns of
              -- the matrix /A/ solved by 'ols'.
           -> Vector
              -- ^ Responder vector.  Must have the same length as the
              -- predictor vectors.
           -> (Vector, Double)
olsRegress preds@(_:_) resps
  | any (/=n) ls        = error $ "predictor vector length mismatch " ++
                                  show lss
  | G.length resps /= n = error $ "responder/predictor length mismatch " ++
                                  show (G.length resps, n)
  | otherwise           = (coeffs, rSquare mxpreds resps coeffs)
  where
    coeffs    = ols mxpreds resps
    mxpreds   = transpose .
                fromVector (length lss + 1) n .
                G.concat $ preds ++ [G.replicate n 1]
    lss@(n:ls) = map G.length preds
olsRegress _ _ = error "no predictors given"

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
    F.for 0 i $ \j -> F.unsafeModify s j $ subtract (unsafeIndex r j i * si)
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

-- | Bootstrap a regression function.  Returns both the results of the
-- regression and the requested confidence interval values.
bootstrapRegress
  :: GenIO
  -> Int         -- ^ Number of resamples to compute.
  -> CL Double   -- ^ Confidence level.
  -> ([Vector] -> Vector -> (Vector, Double))
     -- ^ Regression function.
  -> [Vector]    -- ^ Predictor vectors.
  -> Vector      -- ^ Responder vector.
  -> IO (V.Vector (Estimate ConfInt Double), Estimate ConfInt Double)
bootstrapRegress gen0 numResamples cl rgrss preds0 resp0
  | numResamples < 1   = error $ "bootstrapRegress: number of resamples " ++
                                 "must be positive"
  | otherwise = do
  caps <- getNumCapabilities
  gens <- splitGen caps gen0
  done <- newChan
  forM_ (zip gens (balance caps numResamples)) $ \(gen,count) -> forkIO $ do
      v <- V.replicateM count $ do
           let n = U.length resp0
           ixs <- U.replicateM n $ uniformR (0,n-1) gen
           let resp  = U.backpermute resp0 ixs
               preds = map (flip U.backpermute ixs) preds0
           return $ rgrss preds resp
      rnf v `seq` writeChan done v
  (coeffsv, r2v) <- (G.unzip . V.concat) <$> replicateM caps (readChan done)
  let coeffs  = flip G.imap (G.convert coeffss) $ \i x ->
                est x . U.generate numResamples $ \k -> (coeffsv G.! k) G.! i
      r2      = est r2s (G.convert r2v)
      (coeffss, r2s) = rgrss preds0 resp0
      est s v = estimateFromInterval s (w G.! lo, w G.! hi) cl
        where w  = F.sort v
              bounded i = min (U.length w - 1) (max 0 i)
              lo = bounded $ round c
              hi = bounded $ truncate (n - c)
              n  = fromIntegral numResamples
              c  = n * (significanceLevel cl / 2)
  return (coeffs, r2)

-- | Balance units of work across workers.
balance :: Int -> Int -> [Int]
balance numSlices numItems = zipWith (+) (replicate numSlices q)
                                         (replicate r 1 ++ repeat 0)
 where (q,r) = numItems `quotRem` numSlices
