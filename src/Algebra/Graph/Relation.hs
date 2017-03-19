-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of binary relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation (
    -- * Binary relation
    Relation, domain, relation,

    -- * Graph-like properties
    isEmpty, hasVertex, hasEdge, vertexSet, edgeSet,

    -- * Operations
    preset, postset, reflexiveClosure, symmetricClosure, transitiveClosure,
    preorderClosure, gmap, edgeList, edges, fromAdjacencyList
  ) where

import Algebra.Graph.Relation.Internal

import qualified Data.Set as Set

-- | Check if a relation graph is empty.
--
-- @
-- isEmpty 'Algebra.Graph.empty'      == True
-- isEmpty ('Algebra.Graph.vertex' x) == False
-- @
isEmpty :: Relation a -> Bool
isEmpty = null . domain

-- | Check if a relation graph contains a given vertex.
--
-- @
-- hasVertex x 'Algebra.Graph.empty'      == False
-- hasVertex x ('Algebra.Graph.vertex' x) == True
-- @
hasVertex :: Ord a => a -> Relation a -> Bool
hasVertex v = Set.member v . domain

-- | Check if a relation graph contains a given edge.
--
-- @
-- hasEdge x y 'Algebra.Graph.empty'      == False
-- hasEdge x y ('Algebra.Graph.vertex' z) == False
-- hasEdge x y ('Algebra.Graph.edge' x y) == True
-- @
hasEdge :: Ord a => a -> a -> Relation a -> Bool
hasEdge u v = Set.member (u, v) . relation

-- | The set of vertices of a given graph.
--
-- @
-- vertexSet 'Algebra.Graph.empty'      == Set.empty
-- vertexSet . 'Algebra.Graph.vertex'   == Set.singleton
-- vertexSet . 'Algebra.Graph.vertices' == Set.fromList
-- vertexSet . 'Algebra.Graph.clique'   == Set.fromList
-- @
vertexSet :: Ord a => Relation a -> Set.Set a
vertexSet = domain

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.empty
-- edgeSet ('vertex' x) == Set.empty
-- edgeSet ('edge' x y) == Set.singleton (x,y)
-- edgeSet . 'edges'    == Set.fromList
-- @
edgeSet :: Ord a => Relation a -> Set.Set (a, a)
edgeSet = relation

