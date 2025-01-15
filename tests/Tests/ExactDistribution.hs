{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module    : Tests.ExactDistribution
-- Copyright : (c) 2022 Lorenz Minder
-- License   : BSD3
--
-- Maintainer  : lminder@gmx.net
-- Stability   : experimental
-- Portability : portable
--
-- Tests comparing distributions to exact versions.
--
-- This module provides exact versions of some distributions, and tests
-- to compare them to the production implementations in
-- Statistics.Distribution.*.  It also contains the functionality to
-- test the production distributions against the exact versions.  Errors
-- are flagged if data points are discovered where the probability mass
-- function, the cumulative probability function, or its complement
-- deviates too far (more than a prescribed tolerance) from the exact
-- calculation.
--
-- The distributions here are implemented with rational integer
-- arithmetic, using pretty much the textbook definitions formulas.
-- Numerical problems like overflow or rounding errors cannot occur with
-- this approach, making them are easy to write, read and verify.  They
-- are, of course, substantially slower than the production
-- distributions in Statistics.Distribution.*.  This makes them
-- unsuitable for most uses other than testing and debugging.  (Also,
-- only a handful of distributions can be implemented exactly with
-- rational arithmetic.)
--
-- This module has the following sub-components:
-- 
-- * Exact (rational) definitions of some distribution functions,
--   including both the probability mass as well as the CDF.
--
-- * QC.Arbitrary implementations to sample test cases (i.e.,
--   distribution parameters and evaluation points).
--
-- * "Linkage": a mechanism to construct a production distribution
--   corresponding to a test case for an exact distribution.
--
-- * A set of tests for the distributions derived using all of the above
--   components.
--
-- This module exports a number symbols which can be useful for
-- debugging and experimentation.  For use in a test suite, only the
-- `exactDistributionTests` function is needed.

module Tests.ExactDistribution (
    -- * Exact math functions
      exactChoose

    -- * Exact distributions
    , ExactDiscreteDistr(..)

    , ExactBinomialDistr(..)
    , ExactDiscreteUniformDistr(..)
    , ExactGeometricDistr(..)
    , ExactHypergeomDistr(..)

    -- * Linking to production distributions
    , ProductionLinkage

    -- * Individual test routines
    , pmfMatch
    , cdfMatch
    , complCdfMatch

    -- * Test groups
    , Tag(..)
    , distTests
    , exactDistributionTests
) where

----------------------------------------------------------------

import Data.Foldable
import Data.Ratio

import Test.Tasty                       (TestTree, testGroup)
import Test.Tasty.QuickCheck            (testProperty)
import Test.QuickCheck as QC
import Numeric.MathFunctions.Comparison (relativeError)
import Numeric.MathFunctions.Constants  (m_tiny)

import Statistics.Distribution
import Statistics.Distribution.Binomial
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Geometric
import Statistics.Distribution.Hypergeometric

----------------------------------------------------------------
--
-- Math functions.
--
-- Used for implementing the distributions below.
--
----------------------------------------------------------------

-- | Exactly compute binomial coefficient.
--
-- /n/ need not be an integer, can be fractional.
exactChoose :: Ratio Integer -> Integer -> Ratio Integer
exactChoose n k
    | k < 0     = 0
    | otherwise = foldl' (*) 1 factors
    where   factors = [ (n - k' + j) / j | j <- [1..k'] ]
            k' = fromInteger k :: Ratio Integer

----------------------------------------------------------------
--
-- Exact distributions.
--
----------------------------------------------------------------

-- | Exact discrete distribution.
class ExactDiscreteDistr a where
    -- | Probability mass function.
    exactProb :: a -> Integer -> Ratio Integer
    exactProb d x = exactCumulative d x - exactCumulative d (x - 1)

    -- | Cumulative distribution function.
    exactCumulative :: a -> Integer -> Ratio Integer

-- | Exact Binomial distribution.
data ExactBinomialDistr = ExactBD Integer (Ratio Integer)
    deriving(Show)

instance ExactDiscreteDistr ExactBinomialDistr where
    -- Probability mass, computed with textbook formula.
    exactProb (ExactBD n p) k
        | k < 0 || k > n    = 0
        | otherwise         = exactChoose n' k * p^k * (1-p)^(n-k)
        where n' = fromIntegral n
    -- CDF 
    --
    -- Computed iteratively by summing up all the probabilities
    -- <= /k/.  Rather than computing everything from scratch for each
    -- probability, we reuse previous results.  The meanings of the
    -- variables in the "update" function are:
    -- 
    -- bc   is the binomial coefficient (n choose j),
    -- pj   is the term p^j,
    -- pnj  is the term (1 - p)^(n - j)
    -- r    is the (partial) sum of the probabilities 
    --
    exactCumulative (ExactBD n p) k
        | k < 0             = 0
        | k >= n            = 1
        -- Special case for p = 1, since in the below fold we
        -- divide by (1 - p).
        | p == 1            = if k == n then 1 else 0
        | otherwise
          = result $ foldl' update (1, 1, (1 - p)^n, (1 - p)^n) [1..k]
          where update (!bc, !pj, !pnj, !r) !j =
                    let bc' = bc * (n - j + 1) `div` j 
                        pj' = pj * p
                        pnj' = pnj / (1 - p)
                        r' = r + (fromIntegral bc') * pj' * pnj'
                    in  (bc', pj', pnj', r')
                result (_, _, _, r) = r

-- | Exact Discrete Uniform distribution.
data ExactDiscreteUniformDistr = ExactDU Integer Integer
    deriving(Show)

instance ExactDiscreteDistr ExactDiscreteUniformDistr  where
    exactProb (ExactDU lower upper) k
        | k < lower || k > upper    = 0
        | otherwise                 = 1 % (upper - lower + 1)
    exactCumulative (ExactDU lower upper) k
        | k < lower                 = 0
        | k > upper                 = 1
        | otherwise                 =
            let d = (k - lower + 1)
            in  d % (upper - lower + 1)

-- | Geometric distribution.
data ExactGeometricDistr = ExactGeom (Ratio Integer)
    deriving(Show)

instance ExactDiscreteDistr ExactGeometricDistr where
    exactProb (ExactGeom p) k
        | k < 1                     = 0
        | otherwise                 = (1 - p)^(k - 1) * p

    exactCumulative (ExactGeom p) k = 1 - (1 - p)^k

-- | Hypergeometric distribution.
--
--   Parameters are /K/, /N/ and /n/, where:
--   - /N/ is the total sample space size.
--   - /K/ is number of "good" objects among /N/.
--   - /n/ is the number of draws without replacement.
data ExactHypergeomDistr = ExactHG Integer Integer Integer
    deriving(Show)

instance ExactDiscreteDistr ExactHypergeomDistr where
    exactProb (ExactHG nK nN n) k
        | k < 0                     = 0
        | k > n || k > nN           = 0
        | otherwise                 =
            exactChoose nK' k * exactChoose (nN' - nK') (n - k)
                / exactChoose nN' n
            where nN' = fromIntegral nN
                  nK' = fromIntegral nK

    exactCumulative d k = sum [ exactProb d i | i <- [0..k] ]

----------------------------------------------------------------
--
-- TestCase construction.
--
-- Contains the TestCase data type which encapsulates an instance of an
-- exact distribution together with an evaluation point.
--
-- Then in contains the QC.Arbitrary implementations for TestCases of
-- the different exact distributions.  As a general rule, we try the
-- sampling to be relatively efficient, i.e., we only want to sample
-- valid distribution parameters.  The evaluation points are sampled
-- such that most points are within the support of the distribution.
--
----------------------------------------------------------------

-- Divisor to compute a rational number from an integer.
--
-- We want input parameters to be exactly representable as
-- Double values.  This is so that the production distribution does not
-- mismatch the exact one simply because the input values don't exactly
-- match.  (This can happen if the derivative of the distribution
-- function is large.)   For this reason, the gd value needs to be a
-- power of 2, and <= 2^53, since the mantissa of a Double is 53 bits.
--
-- A value of 2^53 gives the most accurate and diverse tests, but the
-- cost is increased running times, as the computed numerators and
-- denominators will become quite large.
gd :: Integer
gd = 2^(16 :: Int)

-- TestCase
--
-- Combination of an exact distribution together with an evaluation point.
data TestCase a = TestCase a Integer deriving (Show)

instance QC.Arbitrary (TestCase ExactBinomialDistr) where
    arbitrary = do
        -- This somewhat odd sampling of /n/ is done so that lower
        -- values (<1000) are more often represented as the larger ones.
        n <- (*) <$> chooseInteger (1,1000) <*> chooseInteger(1,2)
        p <- (% gd) <$> chooseInteger (0, gd)
        k <- chooseInteger (-1, n + 1)
        return $ TestCase (ExactBD n p) k
    shrink _ = []

instance QC.Arbitrary (TestCase ExactDiscreteUniformDistr) where
    arbitrary = do
        a <- chooseInteger (-1000, 1000)
        sz <- chooseInteger (1, 1000)
        let b = a + sz
        k <- chooseInteger (a - 10, b + 10)
        return $ TestCase (ExactDU a b) k
    shrink _ = []

instance QC.Arbitrary (TestCase ExactGeometricDistr) where
    arbitrary = do
        p <- (% gd) <$> chooseInteger (1, gd)
        let lim = (floor $ 100 / p) :: Integer
        k <- chooseInteger (0, lim)
        return $ TestCase (ExactGeom p) k
    shrink _ = []

instance QC.Arbitrary (TestCase ExactHypergeomDistr) where
    arbitrary = do
        nN <- chooseInteger (1, 100)        -- XXX lower bound should be 0
        nK <- chooseInteger (0, nN)
        n  <- chooseInteger (1, nN)         -- XXX lower bound should be 0
        k  <- chooseInteger (0, min n nK)
        return $ TestCase (ExactHG nK nN n) k
    shrink _ = []

----------------------------------------------------------------
--
-- Linking to the production distributions
--
-- This section contains the ProductionLinkage typeclass and
-- implementation, that allows to obtain a functions for evaluating
-- the production distribution functions for a corresponding exact
-- distribution.
--
----------------------------------------------------------------

class (ExactDiscreteDistr a, DiscreteDistr (ProdDistrib a)
      ) => ProductionLinkage a where
  type ProdDistrib a
  toProd :: a -> ProdDistrib a

instance ProductionLinkage ExactBinomialDistr where
  type ProdDistrib ExactBinomialDistr = BinomialDistribution
  toProd (ExactBD n p) = binomial (fromIntegral n) (fromRational p)

instance ProductionLinkage ExactDiscreteUniformDistr where
  type ProdDistrib ExactDiscreteUniformDistr = DiscreteUniform
  toProd (ExactDU lower upper) = discreteUniformAB (fromIntegral lower) (fromIntegral upper)

instance ProductionLinkage ExactGeometricDistr where
  type ProdDistrib ExactGeometricDistr = GeometricDistribution
  toProd (ExactGeom p) = geometric $ fromRational p

instance ProductionLinkage ExactHypergeomDistr where
  type ProdDistrib ExactHypergeomDistr = HypergeometricDistribution
  toProd (ExactHG nK nN n) =
    hypergeometric (fromIntegral nK) (fromIntegral nN) (fromIntegral n)


----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- Compare that probabilities agree. If they are denormalized just
-- return True. You can't say much about precision
probabilityAgree :: Double -> Double -> Double -> Bool
probabilityAgree tol pe pa
  | pa < 0      = False
  | pe < 0      = False
  | pe < m_tiny = True
  | otherwise   = relativeError pe pa < tol

-- Check production probability mass function accuracy.
--
-- Inputs: tolerance (max relative error) and test case
pmfMatch :: (Show a, ProductionLinkage a) => Double -> TestCase a -> Property
pmfMatch tol (TestCase dExact k)
  = counterexample ("Exact  = " ++ show pe)
  $ counterexample ("Approx = " ++ show pa)
  $ probabilityAgree tol pe pa
  where
    pe = fromRational $ exactProb dExact k
    pa = probability (toProd dExact) (fromIntegral k)

-- Check production cumulative probability function accuracy.
--
-- Inputs:  tolerance (max relative error) and test case.
cdfMatch :: (Show a, ProductionLinkage a) => Double -> TestCase a -> Bool
cdfMatch tol (TestCase dExact k)
  = probabilityAgree tol pe pa
  where
    pe = fromRational $ exactCumulative dExact k
    pa = cumulative (toProd dExact) (fromIntegral k)

-- Check production complement cumulative function accuracy.
--
-- Inputs:  tolerance (max relative error) and test case.
complCdfMatch :: (Show a, ProductionLinkage a) => Double -> TestCase a -> Bool
complCdfMatch tol (TestCase dExact k)
  = probabilityAgree tol pe pa
  where
    pe = fromRational $ 1 - exactCumulative dExact k
    pa = complCumulative (toProd dExact) (fromIntegral k)

-- Phantom type to encode an exact distribution.
data Tag a = Tag

distTests :: forall a. (Show a, ProductionLinkage a, Arbitrary (TestCase a)) =>
    Tag a -> String -> Double -> TestTree
distTests (Tag :: Tag a) name tol =
  testGroup ("Exact tests for " ++ name)
    [ testProperty "PMF match"     $ pmfMatch      @a tol
    , testProperty "CDF match"     $ cdfMatch      @a tol
    , testProperty "1 - CDF match" $ complCdfMatch @a tol
    ]


-- Test driver -------------------------------------------------

exactDistributionTests :: TestTree
exactDistributionTests = testGroup "Test distributions against exact"
  [ distTests (Tag @ExactBinomialDistr)        "Binomial"         1.0e-12
  , distTests (Tag @ExactDiscreteUniformDistr) "DiscreteUniform"  1.0e-12
  , distTests (Tag @ExactGeometricDistr)       "Geometric"        1.0e-13
  , distTests (Tag @ExactHypergeomDistr)       "Hypergeometric"   1.0e-12
  ]
