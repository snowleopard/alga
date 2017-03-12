-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Symmetric
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of binary symmetric relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Symmetric (
    -- * Symmetric relations
    SymmetricRelation, domain, relation,

    -- * Graph-like properties
    isEmpty, hasVertex, hasEdge, toSet,

    -- * Operations
    neighbours, reflexiveClosure, transitiveClosure, preorderClosure, gmap
  ) where

import Algebra.Graph.Relation.Internal (SymmetricRelation (..), symmetricClosure)

import qualified Algebra.Graph.Relation.Internal as R
import qualified Data.Set                        as Set

-- | The /domain/ of the relation.
domain :: SymmetricRelation a -> Set.Set a
domain = R.domain . fromSymmetric

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain, and that if @xRy@ holds then so does @yRx@.
relation :: SymmetricRelation a -> Set.Set (a, a)
relation = R.relation . fromSymmetric

-- | Check if a relation graph is empty.
--
-- @
-- isEmpty 'Algebra.Graph.empty'      == True
-- isEmpty ('Algebra.Graph.vertex' x) == False
-- @
isEmpty :: SymmetricRelation a -> Bool
isEmpty = null . domain

-- | Check if a relation graph contains a given vertex.
--
-- @
-- hasVertex x 'Algebra.Graph.empty'      == False
-- hasVertex x ('Algebra.Graph.vertex' x) == True
-- @
hasVertex :: Ord a => a -> SymmetricRelation a -> Bool
hasVertex v = Set.member v . domain

-- | Check if a relation graph contains a given edge.
--
-- @
-- hasEdge x y 'Algebra.Graph.empty'      == False
-- hasEdge x y ('Algebra.Graph.vertex' z) == False
-- hasEdge x y ('Algebra.Graph.edge' x y) == True
-- @
hasEdge :: Ord a => a -> a -> SymmetricRelation a -> Bool
hasEdge u v = Set.member (u, v) . relation

-- | The set of vertices of a given graph.
--
-- @
-- toSet 'Algebra.Graph.empty'         == Set.empty
-- toSet ('Algebra.Graph.vertex' x)    == Set.singleton x
-- toSet ('Algebra.Graph.vertices' xs) == Set.fromList xs
-- toSet ('Algebra.Graph.clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => SymmetricRelation a -> Set.Set a
toSet = domain

-- | The set of /neighbours/ of an element @x@ is the set of elements that are
-- related to it, i.e. @neighbours x == { a | aRx }@. In the context of undirected
-- graphs, this corresponds to the set of /adjacent/ vertices of vertex @x@.
--
-- @
-- neighbours x 'Algebra.Graph.empty'      == Set.empty
-- neighbours x ('Algebra.Graph.vertex' x) == Set.empty
-- neighbours x ('Algebra.Graph.edge' x y) == Set.fromList [y]
-- neighbours y ('Algebra.Graph.edge' x y) == Set.fromList [x]
-- @
neighbours :: Ord a => a -> SymmetricRelation a -> Set.Set a
neighbours x = R.preset x . symmetricClosure . fromSymmetric

-- | Compute the /reflexive closure/ of a 'SymmetricRelation'.
--
-- @
-- reflexiveClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- reflexiveClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.edge' x x
-- @
reflexiveClosure :: Ord a => SymmetricRelation a -> SymmetricRelation a
reflexiveClosure = SymmetricRelation . R.reflexiveClosure . fromSymmetric

-- | Compute the /transitive closure/ of a 'SymmetricRelation'.
--
-- @
-- transitiveClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- transitiveClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- transitiveClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, x), (x, y), (y, y)]
-- @
transitiveClosure :: Ord a => SymmetricRelation a -> SymmetricRelation a
transitiveClosure = SymmetricRelation . R.transitiveClosure . symmetricClosure . fromSymmetric

-- | Compute the /preorder closure/ of a 'SymmetricRelation'.
--
-- @
-- preorderClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- preorderClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.edge' x x
-- preorderClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, x), (x, y), (y, y)]
-- @
preorderClosure :: Ord a => SymmetricRelation a -> SymmetricRelation a
preorderClosure = SymmetricRelation . R.preorderClosure . symmetricClosure . fromSymmetric

-- | Transform a given relation by applying a function to each of its elements.
-- This is similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'SymmetricRelation'.
--
-- @
-- gmap f 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- gmap f ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' (f x)
-- gmap f ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edge' (f x) (f x)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> SymmetricRelation a -> SymmetricRelation b
gmap f = SymmetricRelation . R.gmap f . symmetricClosure . fromSymmetric
