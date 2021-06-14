-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Transitive
-- Copyright  : (c) Andrey Mokhov 2016-2021
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

import Algebra.Graph.Relation
import Control.DeepSeq
import Data.String

import qualified Algebra.Graph.Class as C

-- TODO: Optimise the implementation by caching the results of transitive closure.
{-| The 'TransitiveRelation' data type represents a /transitive binary relation/
over a set of elements. Transitive relations satisfy all laws of the
'Transitive' type class and, in particular, the /closure/ axiom:

@y /= 'empty' ==> x * y + x * z + y * z == x * y + y * z@

For example, the following holds:

@'path' xs == ('clique' xs :: TransitiveRelation Int)@

The 'Show' instance produces transitively closed expressions:

@show (1 * 2         :: TransitiveRelation Int) == "edge 1 2"
show (1 * 2 + 2 * 3 :: TransitiveRelation Int) == "edges [(1,2),(1,3),(2,3)]"@
-}
newtype TransitiveRelation a = TransitiveRelation { fromTransitive :: Relation a }
    deriving (IsString, NFData, Num)

instance Ord a => Eq (TransitiveRelation a) where
    x == y = toRelation x == toRelation y

instance Ord a => Ord (TransitiveRelation a) where
    compare x y = compare (toRelation x) (toRelation y)

instance (Ord a, Show a) => Show (TransitiveRelation a) where
    show = show . toRelation

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => C.Graph (TransitiveRelation a) where
    type Vertex (TransitiveRelation a) = a
    empty       = TransitiveRelation empty
    vertex      = TransitiveRelation . vertex
    overlay x y = TransitiveRelation $ fromTransitive x `overlay` fromTransitive y
    connect x y = TransitiveRelation $ fromTransitive x `connect` fromTransitive y

instance Ord a => C.Transitive (TransitiveRelation a)

-- | Construct a transitive relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: Relation a -> TransitiveRelation a
fromRelation = TransitiveRelation

-- | Extract the underlying relation.
-- Complexity: /O(n * m * log(m))/ time.
toRelation :: Ord a => TransitiveRelation a -> Relation a
toRelation = transitiveClosure . fromTransitive
