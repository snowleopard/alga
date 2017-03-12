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

    -- * Graph-like properties of binary relations
    isEmpty, hasVertex, hasEdge, toSet,

    -- * Operations on binary relations
    preset, postset, reflexiveClosure, symmetricClosure, transitiveClosure,
    preorderClosure
  ) where

import qualified Data.Set as Set

import Algebra.Graph.Relation.Internal

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
-- toSet 'Algebra.Graph.empty'         == Set.empty
-- toSet ('Algebra.Graph.vertex' x)    == Set.singleton x
-- toSet ('Algebra.Graph.vertices' xs) == Set.fromList xs
-- toSet ('Algebra.Graph.clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => Relation a -> Set.Set a
toSet = domain
