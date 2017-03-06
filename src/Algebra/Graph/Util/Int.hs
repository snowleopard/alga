module Algebra.Graph.Util.Int (VertexSet, vertexSet) where

import qualified Data.IntSet as Set
import Data.IntSet (IntSet)

import Algebra.Graph

newtype VertexSet = VS { vertexSet :: IntSet } deriving (Eq, Show)

instance Graph VertexSet where
    type Vertex VertexSet = Int
    empty       = VS $ Set.empty
    vertex  x   = VS $ Set.singleton x
    overlay x y = VS $ vertexSet x `Set.union` vertexSet y
    connect x y = VS $ vertexSet x `Set.union` vertexSet y

instance Num VertexSet where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id
