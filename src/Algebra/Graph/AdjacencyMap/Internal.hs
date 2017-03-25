-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe. Where possible use non-internal module "Algebra.Graph.AdjacencyMap"
-- instead.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.Internal (
    -- * Adjacency map implementation
    AdjacencyMap (..), consistent
  ) where

import Data.Map.Strict (Map, keysSet, fromSet)
import Data.Set (Set)

import Algebra.Graph.Class

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
show (1 * 2 + 3 :: AdjacencyMap Int) == "graph [1,2,3] [(1,2)]"@

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
newtype AdjacencyMap a = AdjacencyMap {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: Map a (Set a)
  } deriving Eq

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    show (AdjacencyMap m)
        | m == Map.empty = "empty"
        | es == []       = if Set.size vs > 1 then "vertices " ++ show (Set.toAscList vs)
                                              else "vertex "   ++ show v
        | vs == referred = if length es > 1 then "edges " ++ show es
                                            else "edge "  ++ show e ++ " " ++ show f
        | otherwise      = "graph " ++ show (Set.toAscList vs) ++ " " ++ show es
      where
        vs       = keysSet m
        es       = internalEdgeList m
        v        = head $ Set.toList vs
        (e, f)   = head es
        referred = referredToVertexSet m

instance Ord a => Graph (AdjacencyMap a) where
    type Vertex (AdjacencyMap a) = a
    empty       = AdjacencyMap $ Map.empty
    vertex x    = AdjacencyMap $ Map.singleton x Set.empty
    overlay x y = AdjacencyMap $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)
    connect x y = AdjacencyMap $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

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
consistent (AdjacencyMap m) = referredToVertexSet m `Set.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: Ord a => Map a (Set a) -> Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: Map a (Set a) -> [(a, a)]
internalEdgeList m = [ (x, y) | (x, ys) <- Map.toAscList m, y <- Set.toAscList ys ]
