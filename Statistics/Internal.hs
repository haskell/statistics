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
      inlinePerformIO
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

-- | Just like unsafePerformIO, but we inline it. Big performance
-- gains as it exposes lots of things to further inlining. /Very
-- unsafe/. In particular, you should do no memory allocation inside
-- an 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif
