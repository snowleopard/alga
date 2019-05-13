{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.NonEmpty.AdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module exposes the implementation of non-empty adjacency maps. The API
-- is unstable and unsafe, and is exposed only for documentation. You should use
-- the non-internal module "Algebra.Graph.NonEmpty.AdjacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.NonEmpty.AdjacencyMap.Internal (
    -- * Adjacency map implementation
    AdjacencyMap (..), consistent
    ) where

import Control.DeepSeq
import Data.Coerce
import Data.List
import GHC.Generics

import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AM
import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set

{-| The 'AdjacencyMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a 'Num' instance as a convenient notation for
working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

__Note:__ the 'signum' method of the type class 'Num' cannot be implemented and
will throw an error. Furthermore, the 'Num' instance does not satisfy several
"customary laws" of 'Num', which dictate that 'fromInteger' @0@ and
'fromInteger' @1@ should act as additive and multiplicative identities, and
'negate' as additive inverse. Nevertheless, overloading 'fromInteger', '+' and
'*' is very convenient when working with algebraic graphs; we hope that in
future Haskell's Prelude will provide a more fine-grained class hierarchy for
algebraic structures, which we would be able to utilise without violating any
laws.

The 'Show' instance is defined using basic graph construction primitives:

@show (1         :: AdjacencyMap Int) == "vertex 1"
show (1 + 2     :: AdjacencyMap Int) == "vertices1 [1,2]"
show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"
show (1 * 2 * 3 :: AdjacencyMap Int) == "edges1 [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: AdjacencyMap Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance satisfies the following laws of algebraic graphs:

    * 'Algebra.Graph.NonEmpty.AdjacencyMap.overlay' is commutative, associative and idempotent:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z
        >       x + x == x

    * 'Algebra.Graph.NonEmpty.AdjacencyMap.connect' is associative:

        > x * (y * z) == (x * y) * z

    * 'Algebra.Graph.NonEmpty.AdjacencyMap.connect' distributes over 'Algebra.Graph.NonEmpty.AdjacencyMap.overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'Algebra.Graph.NonEmpty.AdjacencyMap.connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

    * 'Algebra.Graph.NonEmpty.AdjacencyMap.connect' satisfies absorption and saturation:

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.

The total order on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Here are a few examples:

@'Algebra.Graph.NonEmpty.AdjacencyMap.vertex' 1 < 'Algebra.Graph.NonEmpty.AdjacencyMap.vertex' 2
'Algebra.Graph.NonEmpty.AdjacencyMap.vertex' 3 < 'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 2
'Algebra.Graph.NonEmpty.AdjacencyMap.vertex' 1 < 'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 1
'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 1 < 'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 2
'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 2 < 'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 1 + 'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 2 2
'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 2 < 'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 3@

Note that the resulting order refines the
'Algebra.Graph.NonEmpty.AdjacencyMap.isSubgraphOf' relation and is compatible
with 'Algebra.Graph.NonEmpty.AdjacencyMap.overlay' and
'Algebra.Graph.NonEmpty.AdjacencyMap.connect' operations:

@'Algebra.Graph.NonEmpty.AdjacencyMap.isSubgraphOf' x y ==> x <= y@

@x     <= x + y
x + y <= x * y@
-}
newtype AdjacencyMap a = NAM {
    -- | The /adjacency map/ of a graph: each vertex is associated with a set of
    -- its direct successors. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- adjacencyMap ('vertex' x) == Map.'Map.singleton' x Set.'Set.empty'
    -- adjacencyMap ('Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 1) == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- adjacencyMap ('Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 2) == Map.'Map.fromList' [(1,Set.'Set.singleton' 2), (2,Set.'Set.empty')]
    -- @
    am :: AM.AdjacencyMap a } deriving (Eq, Generic, NFData, Ord)

-- | __Note:__ this does not satisfy the usual ring laws; see 'AdjacencyMap' for
-- more details.
instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = NAM . AM.vertex . fromInteger
    (+)         = coerce AM.overlay
    (*)         = coerce AM.connect
    signum      = error "NonEmpty.AdjacencyMap.signum cannot be implemented."
    abs         = id
    negate      = id

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    showsPrec p (NAM (AM.AM m))
        | null vs    = error "NonEmpty.AdjacencyMap.Show: Graph is empty"
        | null es    = showParen (p > 10) $ vshow vs
        | vs == used = showParen (p > 10) $ eshow es
        | otherwise  = showParen (p > 10) $
                           showString "overlay (" . vshow (vs \\ used) .
                           showString ") (" . eshow es . showString ")"
      where
        vs             = Set.toAscList (Map.keysSet m)
        es             = AM.internalEdgeList m
        vshow [x]      = showString "vertex "    . showsPrec 11 x
        vshow xs       = showString "vertices1 " . showsPrec 11 xs
        eshow [(x, y)] = showString "edge "      . showsPrec 11 x .
                         showString " "          . showsPrec 11 y
        eshow xs       = showString "edges1 "    . showsPrec 11 xs
        used           = Set.toAscList (AM.referredToVertexSet m)

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices, and the graph is non-empty. It should be
-- impossible to create an inconsistent adjacency map, and we use this function
-- in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent ('vertex' x)    == True
-- consistent ('overlay' x y) == True
-- consistent ('connect' x y) == True
-- consistent ('Algebra.Graph.NonEmpty.AdjacencyMap.edge' x y)    == True
-- consistent ('Algebra.Graph.NonEmpty.AdjacencyMap.edges' xs)    == True
-- consistent ('Algebra.Graph.NonEmpty.AdjacencyMap.stars' xs)    == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (NAM x) = AM.consistent x && not (AM.isEmpty x)
