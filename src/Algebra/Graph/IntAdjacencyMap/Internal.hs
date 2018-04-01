-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.IntAdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
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
    IntAdjacencyMap (..), mkAM, consistent,

    -- * Interoperability with King-Launchbury graphs
    GraphKL (..), mkGraphKL
  ) where

import Data.IntMap.Strict (IntMap, keysSet, fromSet)
import Data.IntSet (IntSet)
import Data.List

import Algebra.Graph.Class

import qualified Data.Graph         as KL
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
data IntAdjacencyMap = AM {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: !(IntMap IntSet),
    -- | Cached King-Launchbury representation.
    -- /Note: this field is for internal use only/.
    graphKL :: GraphKL }

-- | Construct an 'AdjacencyMap' from a map of successor sets and (lazily)
-- compute the corresponding King-Launchbury representation.
-- /Note: this function is for internal use only/.
mkAM :: IntMap IntSet -> IntAdjacencyMap
mkAM m = AM m (mkGraphKL m)

instance Eq IntAdjacencyMap where
    x == y = adjacencyMap x == adjacencyMap y

instance Show IntAdjacencyMap where
    show (AM m _)
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

instance Graph IntAdjacencyMap where
    type Vertex IntAdjacencyMap = Int
    empty       = mkAM   IntMap.empty
    vertex x    = mkAM $ IntMap.singleton x IntSet.empty
    overlay x y = mkAM $ IntMap.unionWith IntSet.union (adjacencyMap x) (adjacencyMap y)
    connect x y = mkAM $ IntMap.unionsWith IntSet.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance Num IntAdjacencyMap where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance ToGraph IntAdjacencyMap where
    type ToVertex IntAdjacencyMap = Int
    toGraph = overlays . map (uncurry star . fmap IntSet.toList) . IntMap.toList . adjacencyMap

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
consistent (AM m _) = referredToVertexSet m `IntSet.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: IntMap IntSet -> IntSet
referredToVertexSet = IntSet.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: IntMap IntSet -> [(Int, Int)]
internalEdgeList m = [ (x, y) | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]

-- | 'GraphKL' encapsulates King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library.
-- /Note: this data structure is for internal use only/.
--
-- If @mkGraphKL (adjacencyMap g) == h@ then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'Algebra.Graph.AdjacencyMap.vertexList' g
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'Algebra.Graph.AdjacencyMap.edgeList' g
-- @
data GraphKL = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    toGraphKL :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @Int@.
    fromVertexKL :: KL.Vertex -> Int,
    -- | A mapping from vertices of type @Int@ to "Data.Graph.Vertex".
    -- Returns 'Nothing' if the argument is not in the graph.
    toVertexKL :: Int -> Maybe KL.Vertex }

-- | Build 'GraphKL' from a map of successor sets.
-- /Note: this function is for internal use only/.
mkGraphKL :: IntMap IntSet -> GraphKL
mkGraphKL m = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, IntSet.toAscList us) | (v, us) <- IntMap.toAscList m ]
