-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.IntAdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe. Where possible use non-internal module
-- "Algebra.Graph.IntAdjacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.IntAdjacencyMap.Internal (
    -- * Adjacency map implementation
    IntAdjacencyMap (..), consistent
  ) where

import Data.IntMap.Strict (IntMap, keysSet, fromSet)
import Data.IntSet (IntSet)

import Algebra.Graph.Class

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
show (1 * 2 + 3 :: IntAdjacencyMap Int) == "graph [1,2,3] [(1,2)]"@

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
newtype IntAdjacencyMap = IntAdjacencyMap {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: IntMap IntSet
  } deriving Eq

instance Show IntAdjacencyMap where
    show (IntAdjacencyMap m)
        | m == IntMap.empty = "empty"
        | es == []          = if IntSet.size vs > 1 then "vertices " ++ show (IntSet.toAscList vs)
                                                    else "vertex "   ++ show v
        | vs == referred    = if length es > 1 then "edges " ++ show es
                                               else "edge "  ++ show e ++ " " ++ show f
        | otherwise         = "graph " ++ show (IntSet.toAscList vs) ++ " " ++ show es
      where
        vs       = keysSet m
        es       = internalEdgeList m
        v        = head $ IntSet.toList vs
        (e, f)   = head es
        referred = referredToVertexSet m

instance Graph IntAdjacencyMap where
    type Vertex IntAdjacencyMap = Int
    empty       = IntAdjacencyMap $ IntMap.empty
    vertex x    = IntAdjacencyMap $ IntMap.singleton x IntSet.empty
    overlay x y = IntAdjacencyMap $ IntMap.unionWith IntSet.union (adjacencyMap x) (adjacencyMap y)
    connect x y = IntAdjacencyMap $ IntMap.unionsWith IntSet.union [ adjacencyMap x, adjacencyMap y,
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
consistent (IntAdjacencyMap m) = referredToVertexSet m `IntSet.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: IntMap IntSet -> IntSet
referredToVertexSet = IntSet.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: IntMap IntSet -> [(Int, Int)]
internalEdgeList m = [ (x, y) | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]
