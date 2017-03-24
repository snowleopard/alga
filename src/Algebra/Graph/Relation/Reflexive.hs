-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Reflexive
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of reflexive binary relations. Use
-- "Algebra.Graph.Class" for polymorphic construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Reflexive (
    -- * Data structure
    ReflexiveRelation, fromRelation, toRelation
  ) where

import Algebra.Graph.Relation.Internal

-- | Construct a reflexive relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: Relation a -> ReflexiveRelation a
fromRelation = ReflexiveRelation

-- | Extract the underlying relation.
-- Complexity: /O(n*log(m))/ time.
toRelation :: Ord a => ReflexiveRelation a -> Relation a
toRelation = reflexiveClosure . fromReflexive
