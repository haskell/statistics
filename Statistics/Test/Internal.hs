{-# LANGUAGE FlexibleContexts #-}
module Statistics.Test.Internal (
    rank
  , rankUnsorted  
  , splitByTags  
  ) where

import Data.Ord
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic.Mutable as M
import Statistics.Function


-- Private data type for unfolding
data Rank v a = Rank {
      rankCnt :: {-# UNPACK #-} !Int        -- Number of ranks to return
    , rankVal :: {-# UNPACK #-} !Double     -- Rank to return
    , rankNum :: {-# UNPACK #-} !Double     -- Current rank
    , rankVec :: v a                        -- Remaining vector
    }

-- | Calculate rank of every element of sample. In case of ties ranks
--   are averaged. Sample should be already sorted in ascending order.
--
--   Rank is index of element in the sample, numeration starts from 1.
--   In case of ties average of ranks of equal elements is assigned
--   to each
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> rank (==) (VU.fromList [10,20,30::Int])
-- [1.0,2.0,3.0]
--
-- >>> rank (==) (VU.fromList [10,10,10,30::Int])
-- [2.0,2.0,2.0,4.0]
rank :: (G.Vector v a)
     => (a -> a -> Bool)        -- ^ Equivalence relation
     -> v a                     -- ^ Vector to rank
     -> U.Vector Double
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

-- | Compute rank of every element of vector. Unlike rank it doesn't
--   require sample to be sorted.
rankUnsorted :: ( Ord a
                , G.Vector v a
                , G.Vector v Int
                , G.Vector v (Int, a)
                )
             => v a
             -> U.Vector Double
rankUnsorted xs = G.create $ do
    -- Put ranks into their original positions
    -- NOTE: backpermute will do wrong thing
    vec <- M.new n
    for 0 n $ \i ->
      M.unsafeWrite vec (index ! i) (ranks ! i)
    return vec
  where
    n = G.length xs
    -- Calculate ranks for sorted array
    ranks = rank (==) sorted
    -- Sort vector and retain original indices of elements
    (index, sorted)
      = G.unzip
      $ sortBy (comparing snd)
      $ indexed xs
{-# INLINE rankUnsorted #-}


-- | Split tagged vector
splitByTags :: (G.Vector v a, G.Vector v (Bool,a)) => v (Bool,a) -> (v a, v a)
splitByTags vs = (G.map snd a, G.map snd b)
  where
    (a,b) = G.unstablePartition fst vs
{-# INLINE splitByTags #-}
