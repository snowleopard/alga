-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Transitive
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of binary transitive relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Transitive (
    -- * Transitive relations
    TransitiveRelation, domain, relation,

    -- * Graph-like properties of binary relations
    isEmpty, hasVertex, hasEdge, toSet,

    -- * Operations on transitive relations
    preset, postset, reflexiveClosure, symmetricClosure, gmap
  ) where

import qualified Data.Set as Set

import qualified Algebra.Graph.Relation.Internal as R
import Algebra.Graph.Relation.Internal (TransitiveRelation (..), transitiveClosure)

-- | The /domain/ of the relation.
domain :: TransitiveRelation a -> Set.Set a
domain = R.domain . fromTransitive

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain, and that if @xRy@ and @yRz@ hold then so does @xRz@.
relation :: Ord a => TransitiveRelation a -> Set.Set (a, a)
relation = R.relation . transitiveClosure . fromTransitive

-- | Check if a relation graph is empty.
--
-- @
-- isEmpty 'Algebra.Graph.empty'      == True
-- isEmpty ('Algebra.Graph.vertex' x) == False
-- @
isEmpty :: TransitiveRelation a -> Bool
isEmpty = null . domain

-- | Check if a relation graph contains a given vertex.
--
-- @
-- hasVertex x 'Algebra.Graph.empty'      == False
-- hasVertex x ('Algebra.Graph.vertex' x) == True
-- @
hasVertex :: Ord a => a -> TransitiveRelation a -> Bool
hasVertex v = Set.member v . domain

-- | Check if a relation graph contains a given edge.
--
-- @
-- hasEdge x y 'Algebra.Graph.empty'      == False
-- hasEdge x y ('Algebra.Graph.vertex' z) == False
-- hasEdge x y ('Algebra.Graph.edge' x y) == True
-- @
hasEdge :: Ord a => a -> a -> TransitiveRelation a -> Bool
hasEdge u v = Set.member (u, v) . relation

-- | The set of vertices of a given graph.
--
-- @
-- toSet 'Algebra.Graph.empty'         == Set.empty
-- toSet ('Algebra.Graph.vertex' x)    == Set.singleton x
-- toSet ('Algebra.Graph.vertices' xs) == Set.fromList xs
-- toSet ('Algebra.Graph.clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => TransitiveRelation a -> Set.Set a
toSet = domain

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- transitive graphs, this corresponds to the set of /transitive predecessors/
-- of vertex @x@.
--
-- @
-- preset x 'Algebra.Graph.empty'            == Set.empty
-- preset x ('Algebra.Graph.vertex' x)       == Set.empty
-- preset x ('Algebra.Graph.edge' x y)       == Set.empty
-- preset y ('Algebra.Graph.edge' x y)       == Set.fromList [x]
-- preset z ('Algebra.Graph.path' [x, y, z]) == Set.fromList [x, y]
-- @
preset :: Ord a => a -> TransitiveRelation a -> Set.Set a
preset x = R.preset x . transitiveClosure . fromTransitive

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- transitive graphs, this corresponds to the set of /transitive successors/
-- of vertex @x@.
--
-- @
-- postset x 'Algebra.Graph.empty'            == Set.empty
-- postset x ('Algebra.Graph.vertex' x)       == Set.empty
-- postset x ('Algebra.Graph.edge' x y)       == Set.fromList [y]
-- postset y ('Algebra.Graph.edge' x y)       == Set.empty
-- postset x ('Algebra.Graph.path' [x, y, z]) == Set.fromList [y, z]
-- @
postset :: Ord a => a -> TransitiveRelation a -> Set.Set a
postset x = R.postset x . transitiveClosure . fromTransitive

-- | Compute the /reflexive closure/ of a 'TransitiveRelation'.
--
-- @
-- reflexiveClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- reflexiveClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.edge' x x
-- @
reflexiveClosure :: Ord a => TransitiveRelation a -> TransitiveRelation a
reflexiveClosure = TransitiveRelation . R.reflexiveClosure . fromTransitive

-- | Compute the /symmetric closure/ of a 'TransitiveRelation'.
--
-- @
-- symmetricClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- symmetricClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- symmetricClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, x), (x, y), (y, x), (y, y)]
-- @
symmetricClosure :: Ord a => TransitiveRelation a -> TransitiveRelation a
symmetricClosure = TransitiveRelation . R.symmetricClosure . transitiveClosure . fromTransitive

-- | Transform a given relation by applying a function to each of its elements.
-- This is similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'TransitiveRelation'.
--
-- @
-- gmap f 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- gmap f ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' (f x)
-- gmap f ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edge' (f x) (f x)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> TransitiveRelation a -> TransitiveRelation b
gmap f = TransitiveRelation . R.gmap f . transitiveClosure . fromTransitive
