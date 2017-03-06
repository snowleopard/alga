{-# LANGUAGE OverloadedStrings #-}
module Algebra.Graph.Todo (Todo, todo, low, high, (~*~), (>*<), priority) where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.String

import Algebra.Graph
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.TopSort

data Todo a = T (Map a Int) (AdjacencyMap a) deriving Show

instance Ord a => Eq (Todo a) where
    x == y = todo x == todo y

instance (IsString a, Ord a) => IsString (Todo a) where
    fromString = vertex . fromString

-- Lower the priority of items in a given todo list
low :: Todo a -> Todo a
low (T p g) = T (Map.map (subtract 1) p) g

-- Raise the priority of items in a given todo list
high :: Todo a -> Todo a
high (T p g) = T (Map.map (+1) p) g

-- Specify exact priority of items in a given todo list (default 0)
priority :: Int -> Todo a -> Todo a
priority x (T p g) = T (Map.map (const x) p) g

-- Pull the arguments together as close as possible
(~*~) :: Ord a => Todo a -> Todo a -> Todo a
x ~*~ y = low x `connect` high y

-- Repel the arguments as far as possible
(>*<) :: Ord a => Todo a -> Todo a -> Todo a
x >*< y = high x `connect` low y

todo :: forall a. Ord a => Todo a -> Maybe [a]
todo (T p g) = fmap (map snd) . topSort $ gmap prioritise g
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
