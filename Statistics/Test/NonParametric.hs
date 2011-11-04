{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Test.NonParametric
-- Copyright : (c) 2010 Neil Brown
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for performing non-parametric tests (i.e. tests without an assumption
-- of underlying distribution).

-- HADDOCK NOTE
--   &#8321; is 1 subscript
--   &#8322; is 2 subscript
module Statistics.Test.NonParametric
{-# DEPRECATED "Use S.Test.MannWhitneyU and S.Test.WilcoxonT instead" #-}
  ( module Statistics.Test.MannWhitneyU
  , module Statistics.Test.WilcoxonT
  ) where

import Statistics.Test.MannWhitneyU
import Statistics.Test.WilcoxonT
