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
        -> Double               -- ^ Lower bound.
        -> Double               -- ^ Upper bound.
        -> (Double -> Double)   -- ^ Function to find the roots of.
        -> Maybe Double
ridders tol lo hi f
    | flo ~= 0    = Just lo
    | fhi ~= 0    = Just hi
    | flo*fhi > 0 = Nothing -- root is not bracketed
    | otherwise   = go lo flo hi fhi 0 (0::Int)
  where
    go !a !fa !b !fb !x0 !i
        | i >= 50 || s ~= 0  = Nothing
        | i > 0   && x ~= x0 = Just x
        | fc*fx > 0 = if fa*fx < 0
                      then go a fa x fx x (i+1)
                      else go x fx b fb x (i+1)
        | otherwise =      go c fc x fx x (i+1)
      where
        s   = sqrt (fc*fc - fa*fb)
        c   = (a+b) * 0.5
        fc  = f c
        x   = c + dx
        fx  = f x
        dx0 = (c-a) * fc / s
        dx | fa-fb < 0 = -dx0
           | otherwise = dx0
    flo = f lo
    fhi = f hi
    a ~= b = abs (a-b) < abs (a*tol)

-- $references
--
-- * Ridders, C.F.J. (1979) A new algorithm for computing a single
--   root of a real continuous function. /IEEE Trans. Circuits
--   Systems/ 26, 979&#8211;980.
