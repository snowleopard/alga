-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.IntAdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe, and is exposed only for documentation. You should use the
-- non-internal module "Algebra.Graph.IntAdjacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.IntAdjacencyMap.Internal (
    -- * Adjacency map implementation
    IntAdjacencyMap (..), empty, vertex, overlay, connect, fromAdjacencyIntSets,
    consistent
  ) where

import Data.IntMap.Strict (IntMap, keysSet, fromSet)
import Data.IntSet (IntSet)
import Data.List

import Control.DeepSeq (NFData (..))

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet

{-| The 'IntAdjacencyMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a 'Num' instance as a convenient notation for
working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show (empty     :: IntAdjacencyMap Int) == "empty"
show (1         :: IntAdjacencyMap Int) == "vertex 1"
show (1 + 2     :: IntAdjacencyMap Int) == "vertices [1,2]"
show (1 * 2     :: IntAdjacencyMap Int) == "edge 1 2"
show (1 * 2 * 3 :: IntAdjacencyMap Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: IntAdjacencyMap Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'Algebra.Graph.IntAdjacencyMap.overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'Algebra.Graph.IntAdjacencyMap.connect' is associative and has
    'Algebra.Graph.IntAdjacencyMap.empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'Algebra.Graph.IntAdjacencyMap.connect' distributes over
    'Algebra.Graph.IntAdjacencyMap.overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'Algebra.Graph.IntAdjacencyMap.connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'Algebra.Graph.IntAdjacencyMap.overlay' has
    'Algebra.Graph.IntAdjacencyMap.empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'Algebra.Graph.IntAdjacencyMap.connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.
-}
newtype IntAdjacencyMap = AM {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: IntMap IntSet } deriving Eq

instance Show IntAdjacencyMap where
    show (AM m)
        | null vs    = "empty"
        | null es    = vshow vs
        | vs == used = eshow es
        | otherwise  = "overlay (" ++ vshow (vs \\ used) ++ ") (" ++ eshow es ++ ")"
      where
        vs             = IntSet.toAscList (keysSet m)
        es             = internalEdgeList m
        vshow [x]      = "vertex "   ++ show x
        vshow xs       = "vertices " ++ show xs
        eshow [(x, y)] = "edge "     ++ show x ++ " " ++ show y
        eshow xs       = "edges "    ++ show xs
        used           = IntSet.toAscList (referredToVertexSet m)

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.IntAdjacencyMap.isEmpty'     empty == True
-- 'Algebra.Graph.IntAdjacencyMap.hasVertex' x empty == False
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' empty == 0
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   empty == 0
-- @
empty :: IntAdjacencyMap
empty = AM IntMap.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.IntAdjacencyMap.isEmpty'     (vertex x) == False
-- 'Algebra.Graph.IntAdjacencyMap.hasVertex' x (vertex x) == True
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' (vertex x) == 1
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (vertex x) == 0
-- @
vertex :: Int -> IntAdjacencyMap
vertex x = AM $ IntMap.singleton x IntSet.empty

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'Algebra.Graph.IntAdjacencyMap.isEmpty'     (overlay x y) == 'Algebra.Graph.IntAdjacencyMap.isEmpty'   x   && 'Algebra.Graph.IntAdjacencyMap.isEmpty'   y
-- 'Algebra.Graph.IntAdjacencyMap.hasVertex' z (overlay x y) == 'Algebra.Graph.IntAdjacencyMap.hasVertex' z x || 'Algebra.Graph.IntAdjacencyMap.hasVertex' z y
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' (overlay x y) >= 'Algebra.Graph.IntAdjacencyMap.vertexCount' x
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' (overlay x y) <= 'Algebra.Graph.IntAdjacencyMap.vertexCount' x + 'Algebra.Graph.IntAdjacencyMap.vertexCount' y
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (overlay x y) >= 'Algebra.Graph.IntAdjacencyMap.edgeCount' x
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (overlay x y) <= 'Algebra.Graph.IntAdjacencyMap.edgeCount' x   + 'Algebra.Graph.IntAdjacencyMap.edgeCount' y
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' (overlay 1 2) == 2
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (overlay 1 2) == 0
-- @
overlay :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
overlay x y = AM $ IntMap.unionWith IntSet.union (adjacencyMap x) (adjacencyMap y)

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'Algebra.Graph.IntAdjacencyMap.isEmpty'     (connect x y) == 'Algebra.Graph.IntAdjacencyMap.isEmpty'   x   && 'Algebra.Graph.IntAdjacencyMap.isEmpty'   y
-- 'Algebra.Graph.IntAdjacencyMap.hasVertex' z (connect x y) == 'Algebra.Graph.IntAdjacencyMap.hasVertex' z x || 'Algebra.Graph.IntAdjacencyMap.hasVertex' z y
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' (connect x y) >= 'Algebra.Graph.IntAdjacencyMap.vertexCount' x
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' (connect x y) <= 'Algebra.Graph.IntAdjacencyMap.vertexCount' x + 'Algebra.Graph.IntAdjacencyMap.vertexCount' y
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (connect x y) >= 'Algebra.Graph.IntAdjacencyMap.edgeCount' x
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (connect x y) >= 'Algebra.Graph.IntAdjacencyMap.edgeCount' y
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (connect x y) >= 'Algebra.Graph.IntAdjacencyMap.vertexCount' x * 'Algebra.Graph.IntAdjacencyMap.vertexCount' y
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (connect x y) <= 'Algebra.Graph.IntAdjacencyMap.vertexCount' x * 'Algebra.Graph.IntAdjacencyMap.vertexCount' y + 'Algebra.Graph.IntAdjacencyMap.edgeCount' x + 'Algebra.Graph.IntAdjacencyMap.edgeCount' y
-- 'Algebra.Graph.IntAdjacencyMap.vertexCount' (connect 1 2) == 2
-- 'Algebra.Graph.IntAdjacencyMap.edgeCount'   (connect 1 2) == 1
-- @
connect :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
connect x y = AM $ IntMap.unionsWith IntSet.union [ adjacencyMap x, adjacencyMap y,
    fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance Num IntAdjacencyMap where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance NFData IntAdjacencyMap where
    rnf (AM a) = rnf a

-- | Construct a graph from a list of adjacency sets.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyIntSets []                                           == 'Algebra.Graph.IntAdjacencyMap.empty'
-- fromAdjacencyIntSets [(x, IntSet.'IntSet.empty')]                          == 'Algebra.Graph.IntAdjacencyMap.vertex' x
-- fromAdjacencyIntSets [(x, IntSet.'IntSet.singleton' y)]                    == 'Algebra.Graph.IntAdjacencyMap.edge' x y
-- fromAdjacencyIntSets . map (fmap IntSet.'IntSet.fromList') . 'Algebra.Graph.IntAdjacencyMap.adjacencyList' == id
-- 'Algebra.Graph.IntAdjacencyMap.overlay' (fromAdjacencyIntSets xs) (fromAdjacencyIntSets ys)       == fromAdjacencyIntSets (xs ++ ys)
-- @
fromAdjacencyIntSets :: [(Int, IntSet)] -> IntAdjacencyMap
fromAdjacencyIntSets ss = AM $ IntMap.unionWith IntSet.union vs es
  where
    vs = IntMap.fromSet (const IntSet.empty) . IntSet.unions $ map snd ss
    es = IntMap.fromListWith IntSet.union ss

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent 'Algebra.Graph.IntAdjacencyMap.empty'                  == True
-- consistent ('Algebra.Graph.IntAdjacencyMap.vertex' x)             == True
-- consistent ('Algebra.Graph.IntAdjacencyMap.overlay' x y)          == True
-- consistent ('Algebra.Graph.IntAdjacencyMap.connect' x y)          == True
-- consistent ('Algebra.Graph.IntAdjacencyMap.edge' x y)             == True
-- consistent ('Algebra.Graph.IntAdjacencyMap.edges' xs)             == True
-- consistent ('Algebra.Graph.IntAdjacencyMap.graph' xs ys)          == True
-- consistent ('Algebra.Graph.IntAdjacencyMap.fromAdjacencyList' xs) == True
-- @
consistent :: IntAdjacencyMap -> Bool
consistent (AM m) = referredToVertexSet m `IntSet.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: IntMap IntSet -> IntSet
referredToVertexSet = IntSet.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: IntMap IntSet -> [(Int, Int)]
internalEdgeList m = [ (x, y) | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]
