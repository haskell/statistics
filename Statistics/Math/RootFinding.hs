{-# LANGUAGE BangPatterns #-}

-- |
-- Module    : Statistics.Math.RootFinding
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Haskell functions for finding the roots of mathematical functions.

module Statistics.Math.RootFinding
    (
      ridders
    -- * References
    -- $references
    ) where

-- | Use the method of Ridders to compute the roots of a function.
--
-- The function must have opposite signs when evaluated at the lower
-- and upper bounds of the search (i.e. the root must be bracketed).
--
-- The result is 'Nothing' if the root is not bracketed, or if the
-- search fails to converge after 50 iterations.
ridders :: Double               -- ^ Error tolerance.
        -> Double               -- ^ Lower bound for the search.
        -> Double               -- ^ Upper bound for the search.
        -> (Double -> Double)   -- ^ Function to find the roots of.
        -> Maybe Double
ridders tol lo hi f
    | flo ~= 0    = Just lo
    | fhi ~= 0    = Just hi
    | flo*fhi > 0 = Nothing -- root is not bracketed
    | otherwise   = go lo flo hi fhi (0::Int)
  where
    go !a !fa !b !fb !i
        | fn ~= 0 || abs (b-a) < tol = Just n
        | i >= 50                    = Nothing
        | fn*fm < 0 = go n fn m fm (i+1)
        | fn*fa < 0 = go a fa n fn (i+1)
        | otherwise = go n fn b fb (i+1)
      where
        dm  = (b - a) * 0.5
        m   = a + dm
        fm  = f m
        dn  = signum (fb - fa) * dm * fm / sqrt(fm*fm - fa*fb)
        n   = m - signum dn * min (abs dn) (abs dm - 0.5 * tol)
        fn  = f n
    flo = f lo
    fhi = f hi
    a ~= b = abs (a-b) <= abs (a*tol)

-- $references
--
-- * Ridders, C.F.J. (1979) A new algorithm for computing a single
--   root of a real continuous function.
--   /IEEE Transactions on Circuits and Systems/ 26:979&#8211;980.
