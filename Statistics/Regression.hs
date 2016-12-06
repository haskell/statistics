{-# LANGUAGE FunctionalDependencies #-}
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
    , normalRegress
    , weightedNormalRegress
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
import Statistics.Types      (Estimate(..),ConfInt,CL,estimateFromInterval,
                              getPValue, TestStatistic(..), TErr(..), NormalErr(..),
                              estimateTErr, estimateNormErr, normalError)
import Statistics.Sample (mean)
import Statistics.Sample.Internal (sum)
import Statistics.Distribution.FDistribution
import Statistics.Distribution.ChiSquared
import System.Random.MWC (GenIO, uniformR)
import Data.Maybe (fromMaybe, isJust)
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
    coeffs     = ols mxpreds resps
    mxpreds    = transpose .
                 fromVector (length lss + 1) n .
                 G.concat $ preds ++ [G.replicate n 1]
    lss@(n:ls) = map G.length preds
olsRegress _ _ = error "no predictors given"


-- | Special case of WeightedNormalRegress where regression noise/error is
-- assumed to be homoskedastic (i.e. noise in all observations have the same variance).
-- That is, noise e has /var(e) = σ&#0178;I/.
class (WeightedNormalRegress a b) => NormalRegress a b | a -> b, b -> a where
  -- | Perform OLS regression with homoskedastic errors.
  -- See 'weightedNormalRegress' for more details.
  normalRegress :: [Vector] -- ^ list of predictor variables
                -> Vector   -- ^ responder vector
                -> a Double -- ^ coefficient estimate error type
                -> (V.Vector (Estimate a Double)
                   ,(TestStatistic Double b)
                   ,Double)
  normalRegress preds resp err = weightedNormalRegress preds resp Nothing err


instance NormalRegress TErr FDistribution
instance NormalRegress NormalErr ChiSquared

-- | Class with main method 'weightedNormalRegress' that performs a weighted
-- least squares regression with Gaussian noise, e, with /var(e) = σ&#0178;diag(w)/
-- where /w/ is known.
class WeightedNormalRegress a b | a -> b, b -> a where
  -- | Takes degrees of freedom and return a reference
  -- distribution for the overall fit test statistic.
  testDist :: Int -- ^ degrees of freedom 1
           -> Int -- ^ degrees of freedom 2 (optional -- not needed for 'ChiSquared')
           -> b
           
  -- | Returns variance parameter for regression noise/error
  -- component.
  errVar :: Double    -- ^ sum of square residuals
         -> Int       -- ^ residual degrees of freedom
         -> a Double  -- ^ either 'Unknown' ('TErr' datatype) if we are to estimate
                      -- the variance or 'NormalErr Double' if it is known a priori.
         -> Double
         
  -- | Return a vector of type 'Estimate a Double' given coefficient estimates,
  -- error estimates, and (optional) degrees of freedom.
  estimateErrs :: Vector -- ^ coefficient estimates
               -> Vector -- ^ coefficient standard errors
               -> Int    -- ^ degrees of freedom (optional -- not needed for 'NormalErr')
               -> V.Vector (Estimate a Double)

  -- | Perform a weighted least squares regression and calculate coefficient
  -- standard errors, goodness-of-fit, and test statistic for overall fit of
  -- the model. Can handle gaussian noise setting when variance is known and
  -- when it is unknown and needs to be estimated
  --
  -- The returned tuple consists of:
  --
  -- * vector of type 'Estimate a Double' which contains point estimates for
  -- regression coefficients, standard errors, and (optional) degrees of freedom.
  --
  -- * overall model test statistic of type 'TestStatistic Double b' which contains
  -- value of test statistic for overall model fit and reference distribution.
  --
  -- * /R&#0178;/, the coefficient of determination (see 'rSquare' for details).
  weightedNormalRegress :: [Vector]     -- ^ list of predictor vectors
                        -> Vector       -- ^ responder vector
                        -> Maybe Vector -- ^ w in /var(e) = σ&#0178;diag(w)/
                        -> a Double     -- ^ coefficient estimate error type
                        -> (V.Vector (Estimate a Double)
                           ,(TestStatistic Double b)
                           ,Double)
  weightedNormalRegress preds@(_:_) resps w err
    | any (/=n) ls        = error $ "predictor vector length mismatch " ++
                                  show lss
    | G.length resps /= n = error $ "responder/predictor length mismatch " ++
                                  show (G.length resps, n)
    | (G.length <$> w) /= Just n
      && isJust w         = error $ "weight vector length mismatch"
    | otherwise           = (est,fitStat,rSq)
    where
      est        = estimateErrs coeffs stdErr df1
      rSq        = 1 - ssr / sst
      fitStat    = TestStatistic ((sst - ssr) / (fromIntegral df1) / sigma2) 
                                 (testDist df1 df2)
      stdErr     = U.map (\x -> sqrt $ x * sigma2) $ varCoeff r
      sigma2     = errVar ssr df2 err
      (ssr,sst0) = sumSquares mxpreds wResps coeffs
      sst        = fromMaybe sst0 $
                   sum <$> U.zipWith (\x y-> square $ y * (x - mean resps)) resps <$> weights
      (coeffs,r) = olsR mxpreds wResps
      df1        = cols mxpreds - 1 
      df2        = n - cols mxpreds 
      mxpreds    = transpose .
                   fromVector (length lss + 1) n .
                   G.concat $ wPreds ++ [fromMaybe (G.replicate n 1) weights]
      weights    = U.map (\s -> 1 / sqrt s) <$> w
      wResps     = fromMaybe resps $ U.zipWith (*) resps <$> weights
      wPreds     = fromMaybe preds $ (\x -> map (U.zipWith (*) x)) <$> weights <*> Just preds
      lss@(n:ls) = map G.length preds
  weightedNormalRegress _ _ _ _ = error "no predictors given"  

-- | Instance of 'WeightedNormalRegress' when noise variance is to be estimated
-- and thus regression coefficients follow a 'StudentT' distribution and overall
-- model fit test statistic has reference distribution 'FDistribution'.
instance WeightedNormalRegress TErr FDistribution where
  testDist df1 df2 = fDistribution df1 df2
  errVar ssr df err
    | err /= Unknown = error $ "for t-distributed errors, error type " ++
                                  "must be specified as Unknown"
    | otherwise      = ssr / (fromIntegral df) 
  estimateErrs ps se df = U.convert $
                          U.zipWith (\p s -> estimateTErr p s (fromIntegral df)) ps se

-- | Instnace of 'WeightedNormalRegerss' when noise variance is known and
-- thus regression coefficients follow a 'NormalDistribution' distribution
-- and overall model fit test statistic has reference distribution 'ChiSquared'.
instance WeightedNormalRegress NormalErr ChiSquared where
  testDist df1 _ = chiSquared df1
  errVar _ _ err = (normalError err)**2
  estimateErrs ps se _ = U.convert $
                         U.zipWith (\p s -> estimateNormErr p s) ps se
                
-- | Compute the ordinary least-squares solution to /A x = b/.
-- We also return the /R/ matrix of the /QR/
-- decompotion of /A/ so that we can compute standard errors.
olsR :: Matrix     -- ^ /A/ has at least as many rows as columns.
     -> Vector     -- ^ /b/ has the same length as columns in /A/.
     -> (Vector,Matrix)
olsR a b
  | rs < cs   = error $ "fewer rows than columns " ++ show d
  | otherwise = (solve (TMatrix r Upper) (transpose q `multiplyV` b),r)
  where
    d@(rs,cs) = dimension a
    (q,r)     = qr a

-- | Compute the ordinary least-squares solution to /A x = b/.
-- This is a wrapper for 
ols :: Matrix     -- ^ /A/ has at least as many rows as columns.
    -> Vector     -- ^ /b/ has the same length as columns in /A/.
    -> Vector
ols a b = fst $ olsR a b

-- | Compute standard errors for regression coefficients
-- in on σ scale
varCoeff :: Matrix -- ^ /R/ part of QR decomposition of design matrix /A/
         -> Vector
varCoeff r = diagOf $ rinv `multiply` (transpose rinv)
  where rinv = inv $ TMatrix r Upper

-- | Efficient inversion of triangular matrix
-- using forward or backward substitution algorithm
inv :: TMatrix -- ^ upper or lower triangular matrix
     -> Matrix  
inv tr = fromColumns $ map (\e -> solve tr e) $ toColumns $ ident n
  where TMatrix r _ = tr
        n = rows r
 
-- | Solve the equation /R x = b/.
solve :: TMatrix    -- ^ /R/ is a (upper or lower) triangular square matrix.
      -> Vector     -- ^ /b/ is of the same length as rows\/columns in /R/.
      -> Vector
solve tr b
  | n /= l    = error $ "row/vector mismatch " ++ show (n,l)
  | otherwise = U.create $ do
  s <- U.thaw b
  rfor n0 n1 $ \i -> do
    si <- (/ unsafeIndex r i i) <$> M.unsafeRead s i
    M.unsafeWrite s i si
    for 0 i $ \j -> F.unsafeModify s j $ subtract (unsafeIndex r j i * si)
  return s
  where TMatrix r ul = tr
        n  = rows r
        n0 = if ul == Upper
               then (fst . U.head $ U.dropWhile (\x -> snd x == 0) $
                                        U.reverse $ U.indexed b) + 1
                     -- ^ Starting at the last non-zero element of b provides a constant
                     -- factor speed up when using solve to perform backward substitution
                     -- to invert upper-triangualr matrices.
               else (fst . U.head $ U.dropWhile (\x -> snd x == 0) $
                                        U.indexed b)
        n1 = if ul == Upper
               then 0
               else n
        l  = U.length b

-- | Compute sum of squares decomposition for a given
-- fitted regression model.
--
-- The return pair consists of:
--
-- * residual sum of squares (/SSR/).
--
-- * total sum of squares (/SST/).
--
-- model sum of squares (/SSM/) can be derived
-- from the identity /SST = SSR + SSM/.
sumSquares :: Matrix               -- ^ Predictors (regressors).
           -> Vector               -- ^ Responders.
           -> Vector               -- ^ Regression coefficients.
           -> (Double,Double)
sumSquares pred resp coeff = (r,t)
  where
    r   = sum $ flip U.imap resp $ \i x -> square (x - p i)
    t   = sum $ flip U.map resp $ \x -> square (x - mean resp)
    p i = sum . flip U.imap coeff $ \j -> (* unsafeIndex pred i j)

-- | Compute /R&#0178;/, the coefficient of determination that
-- indicates goodness-of-fit of a regression.
--
-- This value will be 1 if the predictors fit perfectly, dropping to 0
-- if they have no explanatory power.
rSquare :: Matrix    -- ^ Predictors (regressors).
        -> Vector    -- ^ Responders.
        -> Vector    -- ^ Regression coefficients
        -> Double
rSquare pred resp coeff = 1 - r / t
  where (r,t) = sumSquares pred resp coeff

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
      -- FIXME: CL semantics!
      est s v = estimateFromInterval s (w G.! lo, w G.! hi) cl
        where w  = F.sort v
              lo = round c
              hi = truncate (n - c)
              n  = fromIntegral numResamples
              c  = n * (getPValue cl / 2)
  return (coeffs, r2)

-- | Balance units of work across workers.
balance :: Int -> Int -> [Int]
balance numSlices numItems = zipWith (+) (replicate numSlices q)
                                         (replicate r 1 ++ repeat 0)
 where (q,r) = numItems `quotRem` numSlices
