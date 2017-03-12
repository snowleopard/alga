-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Preorder
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of binary preorder relations.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Preorder (
    -- * Preorder relations
    PreorderRelation, domain, relation,

    -- * Graph-like properties
    isEmpty, hasVertex, hasEdge, toSet,

    -- * Operations
    preset, postset, symmetricClosure, gmap
  ) where

import Algebra.Graph.Relation.Internal (PreorderRelation (..), preorderClosure)

import qualified Algebra.Graph.Relation.Internal as R
import qualified Data.Set                        as Set

-- | The /domain/ of the relation.
domain :: PreorderRelation a -> Set.Set a
domain = R.domain . fromPreorder

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain and is related to itself, and that if
-- @xRy@ and @yRz@ hold then so does @xRz@.
relation :: Ord a => PreorderRelation a -> Set.Set (a, a)
relation = R.relation . preorderClosure . fromPreorder

-- | Check if a relation graph is empty.
--
-- @
-- isEmpty 'Algebra.Graph.empty'      == True
-- isEmpty ('Algebra.Graph.vertex' x) == False
-- @
isEmpty :: PreorderRelation a -> Bool
isEmpty = null . domain

-- | Check if a relation graph contains a given vertex.
--
-- @
-- hasVertex x 'Algebra.Graph.empty'      == False
-- hasVertex x ('Algebra.Graph.vertex' x) == True
-- @
hasVertex :: Ord a => a -> PreorderRelation a -> Bool
hasVertex v = Set.member v . domain

-- | Check if a relation graph contains a given edge.
--
-- @
-- hasEdge x y 'Algebra.Graph.empty'      == False
-- hasEdge x y ('Algebra.Graph.vertex' z) == False
-- hasEdge x y ('Algebra.Graph.edge' x y) == True
-- @
hasEdge :: Ord a => a -> a -> PreorderRelation a -> Bool
hasEdge u v = Set.member (u, v) . relation

-- | The set of vertices of a given graph.
--
-- @
-- toSet 'Algebra.Graph.empty'         == Set.empty
-- toSet ('Algebra.Graph.vertex' x)    == Set.singleton x
-- toSet ('Algebra.Graph.vertices' xs) == Set.fromList xs
-- toSet ('Algebra.Graph.clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => PreorderRelation a -> Set.Set a
toSet = domain

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- preorder graphs, this corresponds to the set of /preorder predecessors/
-- of vertex @x@.
--
-- @
-- preset x 'Algebra.Graph.empty'            == Set.empty
-- preset x ('Algebra.Graph.vertex' x)       == Set.fromList [x]
-- preset x ('Algebra.Graph.edge' x y)       == Set.fromList [x]
-- preset y ('Algebra.Graph.edge' x y)       == Set.fromList [x, y]
-- preset z ('Algebra.Graph.path' [x, y, z]) == Set.fromList [x, y, z]
-- @
preset :: Ord a => a -> PreorderRelation a -> Set.Set a
preset x = R.preset x . preorderClosure . fromPreorder

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- preorder graphs, this corresponds to the set of /preorder successors/
-- of vertex @x@.
--
-- @
-- postset x 'Algebra.Graph.empty'            == Set.empty
-- postset x ('Algebra.Graph.vertex' x)       == Set.fromList [x]
-- postset x ('Algebra.Graph.edge' x y)       == Set.fromList [x, y]
-- postset y ('Algebra.Graph.edge' x y)       == Set.fromList [y]
-- postset x ('Algebra.Graph.path' [x, y, z]) == Set.fromList [x, y, z]
-- @
postset :: Ord a => a -> PreorderRelation a -> Set.Set a
postset x = R.postset x . preorderClosure . fromPreorder

-- | Compute the /symmetric closure/ of a 'PreorderRelation'.
--
-- @
-- symmetricClosure 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- symmetricClosure ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' x
-- symmetricClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, y), (y, x)]
-- @
symmetricClosure :: Ord a => PreorderRelation a -> PreorderRelation a
symmetricClosure = PreorderRelation . R.symmetricClosure . preorderClosure . fromPreorder

-- | Transform a given relation by applying a function to each of its elements.
-- This is similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'PreorderRelation'.
--
-- @
-- gmap f 'Algebra.Graph.empty'      == 'Algebra.Graph.empty'
-- gmap f ('Algebra.Graph.vertex' x) == 'Algebra.Graph.vertex' (f x)
-- gmap f ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edge' (f x) (f x)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> PreorderRelation a -> PreorderRelation b
gmap f = PreorderRelation . R.gmap f . preorderClosure . fromPreorder
