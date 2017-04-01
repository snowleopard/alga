-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Symmetric
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of symmetric binary relations. Use
-- "Algebra.Graph.Class" for polymorphic construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Symmetric (
    -- * Data structure
    SymmetricRelation, fromRelation, toRelation,

    -- * Graph properties
    neighbours
  ) where

import Algebra.Graph.Relation
import Algebra.Graph.Relation.InternalDerived

import qualified Data.Set as Set

-- | Construct a symmetric relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: Relation a -> SymmetricRelation a
fromRelation = SymmetricRelation

-- | Extract the underlying relation.
-- Complexity: /O(m*log(m))/ time.
toRelation :: Ord a => SymmetricRelation a -> Relation a
toRelation = symmetricClosure . fromSymmetric

-- | The set of /neighbours/ of an element @x@ is the set of elements that are
-- related to it, i.e. @neighbours x == { a | aRx }@. In the context of undirected
-- graphs, this corresponds to the set of /adjacent/ vertices of vertex @x@.
--
-- @
-- neighbours x 'Algebra.Graph.Class.empty'      == Set.'Set.empty'
-- neighbours x ('Algebra.Graph.Class.vertex' x) == Set.'Set.empty'
-- neighbours x ('Algebra.Graph.Class.edge' x y) == Set.'Set.fromList' [y]
-- neighbours y ('Algebra.Graph.Class.edge' x y) == Set.'Set.fromList' [x]
-- @
neighbours :: Ord a => a -> SymmetricRelation a -> Set.Set a
neighbours x = postSet x . toRelation
