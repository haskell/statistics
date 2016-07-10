-- |
-- Module    : Statistics.Internal
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- 
module Statistics.Internal (
    -- * Default definitions for Show
    defaultShow1
  , defaultShow2
  , defaultShow3
    -- * Default definitions for Read
  , defaultReadPrec1
  , defaultReadPrec2
  , defaultReadPrec3
    -- * Reexports
  , Show(..)
  , Read(..)
  ) where

import Control.Monad
import Text.Read



----------------------------------------------------------------
-- Default show implementations
----------------------------------------------------------------

defaultShow1 :: (Show a) => String -> a -> Int -> ShowS
defaultShow1 con a n
  = showParen (n >= 11)
  ( showString con
  . showChar ' '
  . showsPrec 11 a
  )

defaultShow2 :: (Show a, Show b) => String -> a -> b -> Int -> ShowS
defaultShow2 con a b n
  = showParen (n >= 11)
  ( showString con
  . showChar ' '
  . showsPrec 11 a
  . showChar ' '
  . showsPrec 11 b
  )

defaultShow3 :: (Show a, Show b, Show c)
             => String -> a -> b -> c -> Int -> ShowS
defaultShow3 con a b c n
  = showParen (n >= 11)
  ( showString con
  . showChar ' '
  . showsPrec 11 a
  . showChar ' '
  . showsPrec 11 b
  . showChar ' '
  . showsPrec 11 c
  )

----------------------------------------------------------------
-- Default read implementations
----------------------------------------------------------------

defaultReadPrec1 :: (Read a) => String -> (a -> r) -> ReadPrec r
defaultReadPrec1 con f = parens $ prec 10 $ do
  Ident con' <- lexP
  guard (con' == con)
  a <- readPrec
  return $ f a

defaultReadPrec2 :: (Read a, Read b) => String -> (a -> b -> r) -> ReadPrec r
defaultReadPrec2 con f = parens $ prec 10 $ do
  Ident con' <- lexP
  guard (con' == con)
  a <- readPrec
  b <- readPrec
  return $ f a b

defaultReadPrec3 :: (Read a, Read b, Read c)
                 => String -> (a -> b -> c -> r) -> ReadPrec r
defaultReadPrec3 con f = parens $ prec 10 $ do
  Ident con' <- lexP
  guard (con' == con)
  a <- readPrec
  b <- readPrec
  c <- readPrec
  return $ f a b c
