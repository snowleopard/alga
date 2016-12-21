{-# LANGUAGE OverloadedStrings #-}
module Algebra.Graph.Todo (Todo, todo, low, high, (><)) where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.String

import Algebra.Graph
import Algebra.Graph.Util

data Todo a = T (Map a Int) (TopSort a) deriving Show

instance Ord a => Eq (Todo a) where
    x == y = todo x == todo y

instance (IsString a, Ord a) => IsString (Todo a) where
    fromString = vertex . fromString

low :: Todo a -> Todo a
low (T p g) = T (Map.map (subtract 1) p) g

high :: Todo a -> Todo a
high (T p g) = T (Map.map (+1) p) g

(><) :: Ord a => Todo a -> Todo a -> Todo a
x >< y = low x `connect` high y

todo :: forall a. Ord a => Todo a -> Maybe [a]
todo (T p g) = fmap (map snd) . topSort $ mapVertices prioritise g
  where
    prioritise :: a -> (Int, a)
    prioritise x = (negate $ Map.findWithDefault 0 x p, x)

instance (IsString a, Ord a) => Num (Todo a) where
    fromInteger i = fromString $ show (fromInteger i :: Integer)
    (+)           = overlay
    (*)           = connect
    signum        = const empty
    abs           = id
    negate        = id

instance Ord a => Graph (Todo a) where
    type Vertex (Todo a) = a
    empty    = T Map.empty empty
    vertex x = T (Map.singleton x 0) (vertex x)
    overlay (T p1 g1) (T p2 g2) = T (Map.unionWith (+) p1 p2) (overlay g1 g2)
    connect (T p1 g1) (T p2 g2) = T (Map.unionWith (+) p1 p2) (connect g1 g2)
