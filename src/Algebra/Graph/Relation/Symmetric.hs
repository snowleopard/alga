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

    -- * Operations on symmetric relations
    neighbours, reflexiveClosure, transitiveClosure, preorderClosure
  ) where

import Data.Set (Set)

import qualified Algebra.Graph.Relation.Internal as R
import Algebra.Graph.Relation.Internal (SymmetricRelation (..), symmetricClosure)

-- | The /domain/ of the relation.
domain :: SymmetricRelation a -> Set a
domain = R.domain . fromSymmetric

-- | The set of pairs of elements that are /related/. It is guaranteed that each
-- element belongs to the domain, and that if @xRy@ holds then so does @yRx@.
relation :: SymmetricRelation a -> Set (a, a)
relation = R.relation . fromSymmetric

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
neighbours :: Ord a => a -> SymmetricRelation a -> Set a
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
