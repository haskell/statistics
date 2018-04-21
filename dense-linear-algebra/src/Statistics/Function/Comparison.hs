-- |
-- Module    : Statistics.Function.Comparison
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Approximate floating point comparison, based on Bruce Dawson's
-- \"Comparing floating point numbers\":
-- <http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm>
module Statistics.Function.Comparison
    {-# DEPRECATED "Use Numeric.MathFunctions.Comparison from math-functions" #-}
    (
      within
    ) where
import Numeric.MathFunctions.Comparison (within)
