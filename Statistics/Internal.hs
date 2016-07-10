{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
-- |
-- Module    : Statistics.Internal
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Scary internal functions.

module Statistics.Internal
    (
      accursedUnutterablePerformIO
    ) where

#if __GLASGOW_HASKELL__ >= 611
import GHC.IO (IO(IO))
#else
import GHC.IOBase (IO(IO))
#endif
import GHC.Base (realWorld#)
#if !defined(__GLASGOW_HASKELL__)
import System.IO.Unsafe (unsafePerformIO)
#endif

-- Lifted from Data.ByteString.Internal so we don't introduce an
-- otherwise unnecessary dependency on the bytestring package.

-- | This \"function\" has a superficial similarity to 'unsafePerformIO' but
-- it is in fact a malevolent agent of chaos. It unpicks the seams of reality
-- (and the 'IO' monad) so that the normal rules no longer apply. It lulls you
-- into thinking it is reasonable, but when you are not looking it stabs you
-- in the back and aliases all of your mutable buffers. The carcass of many a
-- seasoned Haskell programmer lie strewn at its feet.
--
-- Witness the trail of destruction:
--
-- * <https://github.com/haskell/bytestring/commit/71c4b438c675aa360c79d79acc9a491e7bbc26e7>
--
-- * <https://github.com/haskell/bytestring/commit/210c656390ae617d9ee3b8bcff5c88dd17cef8da>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3486>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3487>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/7270>
--
-- Do not talk about \"safe\"! You do not know what is safe!
--
-- Yield not to its blasphemous call! Flee traveller! Flee or you will be
-- corrupted and devoured!
{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
accursedUnutterablePerformIO = unsafePerformIO
#endif
