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
  , defaultReadPrecM1
  , defaultReadPrecM2
  , defaultReadPrecM3
    -- * Reexports
  , Show(..)
  , Read(..)
  ) where

import Control.Applicative
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

defaultReadPrecM1 :: (Read a) => String -> (a -> Maybe r) -> ReadPrec r
defaultReadPrecM1 con f = parens $ prec 10 $ do
  expect con
  a <- readPrec
  maybe empty return $ f a

defaultReadPrecM2 :: (Read a, Read b) => String -> (a -> b -> Maybe r) -> ReadPrec r
defaultReadPrecM2 con f = parens $ prec 10 $ do
  expect con
  a <- readPrec
  b <- readPrec
  maybe empty return $ f a b

defaultReadPrecM3 :: (Read a, Read b, Read c)
                 => String -> (a -> b -> c -> Maybe r) -> ReadPrec r
defaultReadPrecM3 con f = parens $ prec 10 $ do
  expect con
  a <- readPrec
  b <- readPrec
  c <- readPrec
  maybe empty return $ f a b c

expect :: String -> ReadPrec ()
expect str = do
  Ident s <- lexP
  guard (s == str)
