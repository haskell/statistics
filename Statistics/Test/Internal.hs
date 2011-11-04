{-# LANGUAGE FlexibleContexts #-}
module Statistics.Test.Internal (
    rank
  , splitByTags  
  ) where

import qualified Data.Vector.Generic as G



-- Private data type for unfolding
data Rank v a = Rank {
      rankCnt :: {-# UNPACK #-} !Int        -- Number of ranks to return
    , rankVal :: {-# UNPACK #-} !Double     -- Rank to return
    , rankNum :: {-# UNPACK #-} !Double     -- Current rank
    , rankVec :: v a                        -- Remaining vector
    }

-- | Calculate rank of sample. Sample should be already sorted.
rank :: (G.Vector v a, G.Vector v Double)
     => (a -> a -> Bool)        -- ^ Equivalence relation
     -> v a                     -- ^ Vector to rank
     -> v Double
rank eq vec = G.unfoldr go (Rank 0 (-1) 1 vec)
  where
    go (Rank 0 _ r v)
      | G.null v  = Nothing
      | otherwise =
          case G.length h of
            1 -> Just (r, Rank 0 0 (r+1) rest)
            n -> go Rank { rankCnt = n
                         , rankVal = 0.5 * (r*2 + fromIntegral (n-1))
                         , rankNum = r + fromIntegral n
                         , rankVec = rest
                         }
          where
            (h,rest) = G.span (eq $ G.head v) v
    go (Rank n val r v) = Just (val, Rank (n-1) val r v)
{-# INLINE rank #-}

-- | Split tagged vector
splitByTags :: (G.Vector v a, G.Vector v (Bool,a)) => v (Bool,a) -> (v a, v a)
splitByTags vs = (G.map snd a, G.map snd b)
  where
    (a,b) = G.unstablePartition fst vs
{-# INLINE splitByTags #-}
