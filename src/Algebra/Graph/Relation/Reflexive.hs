-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Reflexive
-- Copyright  : (c) Andrey Mokhov 2016-2019
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

import Control.DeepSeq
import Algebra.Graph.Relation

import qualified Algebra.Graph.Class as C

{-| The 'ReflexiveRelation' data type represents a /reflexive binary relation/
over a set of elements. Reflexive relations satisfy all laws of the
'Reflexive' type class and, in particular, the /self-loop/ axiom:

@'vertex' x == 'vertex' x * 'vertex' x@

The 'Show' instance produces reflexively closed expressions:

@show (1     :: ReflexiveRelation Int) == "edge 1 1"
show (1 * 2 :: ReflexiveRelation Int) == "edges [(1,1),(1,2),(2,2)]"@
-}
newtype ReflexiveRelation a = ReflexiveRelation { fromReflexive :: Relation a }
    deriving (Num, NFData)

instance Ord a => Eq (ReflexiveRelation a) where
    x == y = reflexiveClosure (fromReflexive x) == reflexiveClosure (fromReflexive y)

instance (Ord a, Show a) => Show (ReflexiveRelation a) where
    show = show . reflexiveClosure . fromReflexive

instance Ord a => C.Graph (ReflexiveRelation a) where
    type Vertex (ReflexiveRelation a) = a
    empty       = ReflexiveRelation empty
    vertex      = ReflexiveRelation . vertex
    overlay x y = ReflexiveRelation $ fromReflexive x `overlay` fromReflexive y
    connect x y = ReflexiveRelation $ fromReflexive x `connect` fromReflexive y

instance Ord a => C.Reflexive (ReflexiveRelation a)

-- | Construct a reflexive relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: Relation a -> ReflexiveRelation a
fromRelation = ReflexiveRelation

-- | Extract the underlying relation.
-- Complexity: /O(n*log(m))/ time.
toRelation :: Ord a => ReflexiveRelation a -> Relation a
toRelation = reflexiveClosure . fromReflexive
