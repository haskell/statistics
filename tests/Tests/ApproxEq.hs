{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Tests.ApproxEq
    (
      ApproxEq(..)
    ) where

import Data.Complex (Complex(..), realPart)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Numeric.MathFunctions.Constants (m_epsilon)
import Statistics.Matrix hiding (map, toList)
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Matrix as M

class (Eq a, Show a) => ApproxEq a where
    type Bounds a

    eq   :: Bounds a -> a -> a -> Bool
    eql  :: Bounds a -> a -> a -> Property
    eql eps a b = counterexample (show a ++ " /=~ " ++ show b) (eq eps a b)

    (=~)  :: a -> a -> Bool

    (==~) :: a -> a -> Property
    a ==~ b = counterexample (show a ++ " /=~ " ++ show b) (a =~ b)

instance ApproxEq Double where
    type Bounds Double = Double

    eq eps a b
      | a == 0 && b == 0 = True
      | otherwise        = abs (a - b) <= eps * max (abs a) (abs b)
    (=~)  = eq m_epsilon

instance ApproxEq (Complex Double) where
    type Bounds (Complex Double) = Double

    eq eps a@(ar :+ ai) b@(br :+ bi)
      | a == 0 && b == 0 = True
      | otherwise        = abs (ar - br) <= eps * d
                        && abs (ai - bi) <= eps * d
      where
        d = max (realPart $ abs a) (realPart $ abs b)

    (=~)  = eq m_epsilon

instance ApproxEq [Double] where
    type Bounds [Double] = Double

    eq  eps (x:xs) (y:ys) = eq eps x y && eq eps xs ys
    eq  _   []     []     = True
    eq  _   _      _      = False

    eql   = eqll length id id
    (=~)  = eq m_epsilon
    (==~) = eql m_epsilon

instance ApproxEq (U.Vector Double) where
    type Bounds (U.Vector Double) = Double

    eq    = eqv
    (=~)  = eq m_epsilon
    eql   = eqlv
    (==~) = eqlv m_epsilon

instance ApproxEq (V.Vector Double) where
    type Bounds (V.Vector Double) = Double

    eq    = eqv
    (=~)  = eq m_epsilon
    eql   = eqlv
    (==~) = eqlv m_epsilon

instance ApproxEq Matrix where
    type Bounds Matrix = Double

    eq eps (Matrix r1 c1 v1) (Matrix r2 c2 v2) =
      (r1,c1) == (r2,c2) && eq eps v1 v2
    (=~)  = eq m_epsilon
    eql eps a b = eqll dimension M.toList (`quotRem` cols a) eps a b
    (==~) = eql m_epsilon

eqv :: (ApproxEq a, G.Vector v Bool, G.Vector v a) =>
       Bounds a -> v a -> v a -> Bool
eqv eps a b = G.length a == G.length b && G.and (G.zipWith (eq eps) a b)

eqlv :: (ApproxEq [a], G.Vector v a) => Bounds [a] -> v a -> v a -> Property
eqlv eps a b = eql eps (G.toList a) (G.toList b)

eqll :: (ApproxEq l, ApproxEq a, Show c, Show d, Eq d, Bounds l ~ Bounds a) =>
        (l -> d) -> (l -> [a]) -> (Int -> c) -> Bounds l -> l -> l -> Property
eqll dim toList coord eps a b = counterexample fancy $ eq eps a b
  where
    fancy
      | la /= lb  = "size mismatch: " ++ show la ++ " /= " ++ show lb
      | length summary < length full = summary
      | otherwise = full
    summary = concat . intersperse ", " . catMaybes $
              zipWith3 whee (map coord [(0::Int)..]) xs ys
    full | '\n' `elem` sa = sa ++ "  /=~\n" ++ sb
         | otherwise      = sa ++ " /=~" ++ sb
    (sa, sb) = (show a, show b)
    (xs, ys) = (toList a, toList b)
    (la, lb) = (dim a, dim b)
    whee i x y | eq eps x y = Nothing
               | otherwise  = Just $ show i ++ ": " ++ show x ++ " /=~ " ++ show y
