{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Matrix.Types
    (
      Mat(..)
    , fromMat
    , toMat
    , arbMat
    , arbMatWith
    ) where

import Control.Monad (join)
import Control.Applicative ((<$>), (<*>))
import Statistics.Matrix (Matrix(..), fromList)
import Test.QuickCheck
import Tests.Helpers (shrinkFixedList, small)
import qualified Data.Vector.Unboxed as U

data Mat a = Mat { mrows :: Int , mcols :: Int
                 , asList :: [[a]] }
              deriving (Eq, Ord, Show, Functor)

fromMat :: Mat Double -> Matrix
fromMat (Mat r c xs) = fromList r c (concat xs)

toMat :: Matrix -> Mat Double
toMat (Matrix r c v) = Mat r c . split . U.toList $ v
  where split xs@(_:_) = let (h,t) = splitAt c xs
                         in h : split t
        split []       = []

instance (Arbitrary a) => Arbitrary (Mat a) where
    arbitrary = small $ join (arbMat <$> arbitrary <*> arbitrary)
    shrink (Mat r c xs) = Mat r c <$> shrinkFixedList (shrinkFixedList shrink) xs

arbMat
  :: (Arbitrary a)
  => Positive (Small Int)
  -> Positive (Small Int)
  -> Gen (Mat a)
arbMat r c = arbMatWith r c arbitrary

arbMatWith
  :: (Arbitrary a)
  => Positive (Small Int)
  -> Positive (Small Int)
  -> Gen a
  -> Gen (Mat a)
arbMatWith (Positive (Small r)) (Positive (Small c)) genA =
    Mat r c <$> vectorOf r (vectorOf c genA)

instance Arbitrary Matrix where
    arbitrary = fromMat <$> arbitrary
    -- shrink    = map fromMat . shrink . toMat
