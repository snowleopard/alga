-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Preorder
-- Copyright  : (c) Andrey Mokhov 2016-2024
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

import Algebra.Graph.Relation
import Control.DeepSeq
import Data.String

import qualified Algebra.Graph.Class as C

-- TODO: Optimise the implementation by caching the results of preorder closure.
{-| The 'PreorderRelation' data type represents a
/binary relation that is both reflexive and transitive/. Preorders satisfy all
laws of the 'Preorder' type class and, in particular, the /self-loop/ axiom:

@'vertex' x == 'vertex' x * 'vertex' x@

and the /closure/ axiom:

@y /= 'empty' ==> x * y + x * z + y * z == x * y + y * z@

For example, the following holds:

@'path' xs == ('clique' xs :: PreorderRelation Int)@

The 'Show' instance produces reflexively and transitively closed expressions:

@show (1             :: PreorderRelation Int) == "edge 1 1"
show (1 * 2         :: PreorderRelation Int) == "edges [(1,1),(1,2),(2,2)]"
show (1 * 2 + 2 * 3 :: PreorderRelation Int) == "edges [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]"@
-}
newtype PreorderRelation a = PreorderRelation { fromPreorder :: Relation a }
    deriving (IsString, NFData, Num)

instance (Ord a, Show a) => Show (PreorderRelation a) where
    show = show . toRelation

instance Ord a => Eq (PreorderRelation a) where
    x == y = toRelation x == toRelation y

instance Ord a => Ord (PreorderRelation a) where
    compare x y = compare (toRelation x) (toRelation y)

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => C.Graph (PreorderRelation a) where
    type Vertex (PreorderRelation a) = a
    empty       = PreorderRelation empty
    vertex      = PreorderRelation . vertex
    overlay x y = PreorderRelation $ fromPreorder x `overlay` fromPreorder y
    connect x y = PreorderRelation $ fromPreorder x `connect` fromPreorder y

instance Ord a => C.Reflexive  (PreorderRelation a)
instance Ord a => C.Transitive (PreorderRelation a)
instance Ord a => C.Preorder   (PreorderRelation a)

-- | Construct a preorder relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: Relation a -> PreorderRelation a
fromRelation = PreorderRelation

-- | Extract the underlying relation.
-- Complexity: /O(n * m * log(m))/ time.
toRelation :: Ord a => PreorderRelation a -> Relation a
toRelation = closure . fromPreorder
