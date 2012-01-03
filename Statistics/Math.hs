{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- |
-- Module    : Statistics.Math
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Mathematical functions for statistics.
--
-- DEPRECATED. Use package math-functions instead. This module is just
-- reexports functions from 'Numeric.SpecFunctions',
-- 'Numeric.SpecFunctions.Extra' and 'Numeric.Polynomial.Chebyshev'.

module Statistics.Math
{-# DEPRECATED "Use package math-function" #-} 
    ( module Numeric.Polynomial.Chebyshev
    , module Numeric.SpecFunctions
    , module Numeric.SpecFunctions.Extra
    ) where

import Numeric.Polynomial.Chebyshev
import Numeric.SpecFunctions
import Numeric.SpecFunctions.Extra

