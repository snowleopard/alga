-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Transitive
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of transitive binary relations. Use
-- "Algebra.Graph.Class" for polymorphic construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Transitive (
    -- * Data structure
    TransitiveRelation, fromRelation, toRelation
  ) where

import Algebra.Graph.Relation.Internal

-- | Construct a reflexive relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: Relation a -> TransitiveRelation a
fromRelation = TransitiveRelation

-- | Extract the underlying relation.
-- Complexity: /O(n * m * log(m))/ time.
toRelation :: Ord a => TransitiveRelation a -> Relation a
toRelation = transitiveClosure . fromTransitive
