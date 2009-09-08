-- |
-- Module    : Statistics.Types
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Types for working with statistics.

module Statistics.Types
    (
      Sample
    , Sorted(..)
    , Weights
    ) where

import Data.Array.Vector (UArr)

type Sample = UArr Double
type Weights = UArr Double
    
newtype Sorted a = Sorted { unsort :: a }
    deriving (Eq, Ord, Show)
