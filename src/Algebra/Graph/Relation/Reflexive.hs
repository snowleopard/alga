-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Reflexive
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of binary reflexive relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Reflexive (
    -- * Reflexive relations
    ReflexiveRelation, domain, relation,

    -- * Graph-like properties
    isEmpty, hasVertex, hasEdge, toSet,

    -- * Operations
    preset, postset, symmetricClosure, transitiveClosure, gmap
  ) where

import Algebra.Graph.Relation.Internal (ReflexiveRelation (..), reflexiveClosure)

import qualified Algebra.Graph.Relation.Internal as R
import qualified Data.Set                        as Set

-- | The /domain/ of the relation.
domain :: ReflexiveRelation a -> Set.Set a
domain = R.domain . fromReflexive

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain and is related to itself.
relation :: ReflexiveRelation a -> Set.Set (a, a)
relation = R.relation . fromReflexive

-- | Check if a relation graph is empty.
--
-- @
-- isEmpty 'Algebra.Graph.empty'      == True
-- isEmpty ('Algebra.Graph.vertex' x) == False
-- @
isEmpty :: ReflexiveRelation a -> Bool
isEmpty = null . domain

-- | Check if a relation graph contains a given vertex.
--
-- @
-- hasVertex x 'Algebra.Graph.empty'      == False
-- hasVertex x ('Algebra.Graph.vertex' x) == True
-- @
hasVertex :: Ord a => a -> ReflexiveRelation a -> Bool
hasVertex v = Set.member v . domain

-- | Check if a relation graph contains a given edge.
--
-- @
-- hasEdge x y 'Algebra.Graph.empty'      == False
-- hasEdge x y ('Algebra.Graph.vertex' z) == False
-- hasEdge x y ('Algebra.Graph.edge' x y) == True
-- @
hasEdge :: Ord a => a -> a -> ReflexiveRelation a -> Bool
hasEdge u v = Set.member (u, v) . relation

-- | The set of vertices of a given graph.
--
-- @
-- toSet 'Algebra.Graph.empty'         == Set.empty
-- toSet ('Algebra.Graph.vertex' x)    == Set.singleton x
-- toSet ('Algebra.Graph.vertices' xs) == Set.fromList xs
-- toSet ('Algebra.Graph.clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => ReflexiveRelation a -> Set.Set a
toSet = domain

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- graphs, this corresponds to the set of /direct predecessors/ of vertex @x@.
--
-- @
-- preset x 'Algebra.Graph.empty'      == Set.empty
-- preset x ('Algebra.Graph.vertex' x) == Set.fromList [x]
-- preset x ('Algebra.Graph.edge' x y) == Set.fromList [x]
-- preset y ('Algebra.Graph.edge' x y) == Set.fromList [x, y]
-- @
preset :: Ord a => a -> ReflexiveRelation a -> Set.Set a
preset x = R.preset x . reflexiveClosure . fromReflexive

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- graphs, this corresponds to the set of /direct successors/ of vertex @x@.
--
-- @
-- postset x 'Algebra.Graph.empty'      == Set.empty
-- postset x ('Algebra.Graph.vertex' x) == Set.fromList [x]
-- postset x ('Algebra.Graph.edge' x y) == Set.fromList [x, y]
-- postset y ('Algebra.Graph.edge' x y) == Set.fromList [y]
-- @
postset :: Ord a => a -> ReflexiveRelation a -> Set.Set a
postset x = R.postset x . reflexiveClosure . fromReflexive

-- | Compute the /symmetric closure/ of a 'ReflexiveRelation'.
--
-- @
-- symmetricClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- symmetricClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- symmetricClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, y), (y, x)]
-- @
symmetricClosure :: Ord a => ReflexiveRelation a -> ReflexiveRelation a
symmetricClosure = ReflexiveRelation . R.symmetricClosure . fromReflexive

-- | Compute the /transitive closure/ of a 'ReflexiveRelation'.
--
-- @
-- transitiveClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- transitiveClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- transitiveClosure ('Algebra.Graph.path' xs)  == 'Algebra.Graph.clique' xs
-- @
transitiveClosure :: Ord a => ReflexiveRelation a -> ReflexiveRelation a
transitiveClosure = ReflexiveRelation . R.transitiveClosure . fromReflexive

-- | Transform a given relation by applying a function to each of its elements.
-- This is similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'ReflexiveRelation'.
--
-- @
-- gmap f 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- gmap f ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' (f x)
-- gmap f ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edge' (f x) (f x)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> ReflexiveRelation a -> ReflexiveRelation b
gmap f = ReflexiveRelation . R.gmap f . reflexiveClosure . fromReflexive
