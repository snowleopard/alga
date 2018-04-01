-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe, and is exposed only for documentation. You should use the
-- non-internal module "Algebra.Graph.AdjacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.Internal (
    -- * Adjacency map implementation
    AdjacencyMap (..), mkAM, consistent,

    -- * Interoperability with King-Launchbury graphs
    GraphKL (..), mkGraphKL
  ) where

import Data.List
import Data.Map.Strict (Map, keysSet, fromSet)
import Data.Set (Set)

import Algebra.Graph.Class

import qualified Data.Graph      as KL
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

{-| The 'AdjacencyMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a 'Num' instance as a convenient notation for
working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show (empty     :: AdjacencyMap Int) == "empty"
show (1         :: AdjacencyMap Int) == "vertex 1"
show (1 + 2     :: AdjacencyMap Int) == "vertices [1,2]"
show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"
show (1 * 2 * 3 :: AdjacencyMap Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: AdjacencyMap Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'Algebra.Graph.AdjacencyMap.overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'Algebra.Graph.AdjacencyMap.connect' is associative and has
    'Algebra.Graph.AdjacencyMap.empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'Algebra.Graph.AdjacencyMap.connect' distributes over
    'Algebra.Graph.AdjacencyMap.overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'Algebra.Graph.AdjacencyMap.connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'Algebra.Graph.AdjacencyMap.overlay' has 'Algebra.Graph.AdjacencyMap.empty'
    as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'Algebra.Graph.AdjacencyMap.connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.
-}
data AdjacencyMap a = AM {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: !(Map a (Set a)),
    -- | Cached King-Launchbury representation.
    -- /Note: this field is for internal use only/.
    graphKL :: GraphKL a }

-- | Construct an 'AdjacencyMap' from a map of successor sets and (lazily)
-- compute the corresponding King-Launchbury representation.
-- /Note: this function is for internal use only/.
mkAM :: Ord a => Map a (Set a) -> AdjacencyMap a
mkAM m = AM m (mkGraphKL m)

instance Eq a => Eq (AdjacencyMap a) where
    x == y = adjacencyMap x == adjacencyMap y

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    show (AM m _)
        | null vs    = "empty"
        | null es    = vshow vs
        | vs == used = eshow es
        | otherwise  = "overlay (" ++ vshow (vs \\ used) ++ ") (" ++ eshow es ++ ")"
      where
        vs             = Set.toAscList (keysSet m)
        es             = internalEdgeList m
        vshow [x]      = "vertex "   ++ show x
        vshow xs       = "vertices " ++ show xs
        eshow [(x, y)] = "edge "     ++ show x ++ " " ++ show y
        eshow xs       = "edges "    ++ show xs
        used           = Set.toAscList (referredToVertexSet m)

instance Ord a => Graph (AdjacencyMap a) where
    type Vertex (AdjacencyMap a) = a
    empty       = mkAM   Map.empty
    vertex x    = mkAM $ Map.singleton x Set.empty
    overlay x y = mkAM $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)
    connect x y = mkAM $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance ToGraph (AdjacencyMap a) where
    type ToVertex (AdjacencyMap a) = a
    toGraph = overlays . map (uncurry star . fmap Set.toList) . Map.toList . adjacencyMap

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent 'Algebra.Graph.AdjacencyMap.empty'                  == True
-- consistent ('Algebra.Graph.AdjacencyMap.vertex' x)             == True
-- consistent ('Algebra.Graph.AdjacencyMap.overlay' x y)          == True
-- consistent ('Algebra.Graph.AdjacencyMap.connect' x y)          == True
-- consistent ('Algebra.Graph.AdjacencyMap.edge' x y)             == True
-- consistent ('Algebra.Graph.AdjacencyMap.edges' xs)             == True
-- consistent ('Algebra.Graph.AdjacencyMap.graph' xs ys)          == True
-- consistent ('Algebra.Graph.AdjacencyMap.fromAdjacencyList' xs) == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (AM m _) = referredToVertexSet m `Set.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: Ord a => Map a (Set a) -> Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: Map a (Set a) -> [(a, a)]
internalEdgeList m = [ (x, y) | (x, ys) <- Map.toAscList m, y <- Set.toAscList ys ]

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
data GraphKL a = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    toGraphKL :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @a@.
    fromVertexKL :: KL.Vertex -> a,
    -- | A mapping from vertices of type @a@ to "Data.Graph.Vertex".
    -- Returns 'Nothing' if the argument is not in the graph.
    toVertexKL :: a -> Maybe KL.Vertex }

-- | Build 'GraphKL' from a map of successor sets.
-- /Note: this function is for internal use only/.
mkGraphKL :: Ord a => Map a (Set a) -> GraphKL a
mkGraphKL m = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, Set.toAscList us) | (v, us) <- Map.toAscList m ]
