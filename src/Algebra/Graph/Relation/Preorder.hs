-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Preorder
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of preorder relations. Use "Algebra.Graph.Class"
-- for polymorphic construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Preorder (
    -- * Data structure
    PreorderRelation, fromRelation, toRelation
  ) where

import Algebra.Graph.Relation.Internal

-- | Construct a reflexive relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: Relation a -> PreorderRelation a
fromRelation = PreorderRelation

-- | Extract the underlying relation.
-- Complexity: /O(n * m * log(m))/ time.
toRelation :: Ord a => PreorderRelation a -> Relation a
toRelation = preorderClosure . fromPreorder

