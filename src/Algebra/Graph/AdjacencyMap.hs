{-# LANGUAGE TypeFamilies #-}
module Algebra.Graph.AdjacencyMap (AdjacencyMap (..)) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Algebra.Graph

data AdjacencyMap a = AdjacencyMap { adj :: Map a (Set a) }
    deriving (Eq, Show)

instance Ord a => Graph (AdjacencyMap a) where
    type Vertex (AdjacencyMap a) = a
    empty       = AdjacencyMap Map.empty
    vertex  x   = AdjacencyMap $ Map.singleton x Set.empty
    overlay x y = AdjacencyMap $ Map.unionWith Set.union (adj x) (adj y)
    connect x y = AdjacencyMap $ Map.unionsWith Set.union [ adj x, adj y,
        Map.fromSet (const . Map.keysSet $ adj y) (Map.keysSet $ adj x) ]

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id
