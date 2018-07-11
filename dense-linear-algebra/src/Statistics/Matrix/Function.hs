{-# LANGUAGE BangPatterns #-}
-- |
module Statistics.Matrix.Function where

-- | Multiply a number by itself.
square :: Double -> Double
square x = x * x

-- | Simple for loop.  Counts from /start/ to /end/-1.
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 !n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = f i >> loop (i+1)
{-# INLINE for #-}
