{-# LANGUAGE ConstrainedClassMethods #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.ToGraph
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines ...
--
-----------------------------------------------------------------------------
module Algebra.Graph.ToGraph (
    ToGraph (..)
  ) where

import Prelude ()
import Prelude.Compat

import qualified Algebra.Graph                       as G
import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AM
import qualified Algebra.Graph.IntAdjacencyMap       as IAM
import qualified Algebra.Graph.Relation              as R
import qualified Data.IntMap                         as IntMap
import qualified Data.IntSet                         as IntSet
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set

-- | The 'ToGraph' type class captures data types that can be converted to
-- algebraic graphs.
class ToGraph t where
    {-# MINIMAL toGraph | foldg #-}
    type ToVertex t

    -- | Convert a value to an algebraic graph, see "Algebra.Graph".
    --
    -- @
    -- toGraph == 'foldg' 'G.Empty' 'G.Vertex' 'G.Overlay' 'G.Connect'
    -- @
    toGraph :: t -> G.Graph (ToVertex t)
    toGraph = foldg G.Empty G.Vertex G.Overlay G.Connect

    -- | The method 'foldg' is used for generalised graph folding. It recursively
    -- collapses a given data type by applying the provided graph construction
    -- primitives. The order of arguments is: empty, vertex, overlay and connect,
    -- and it is assumed that the functions satisfy the axioms of the algebra.
    --
    -- @
    -- foldg == 'G.foldg' . 'toGraph'
    -- @
    foldg :: r -> (ToVertex t -> r) -> (r -> r -> r) -> (r -> r -> r) -> t -> r
    foldg e v o c = G.foldg e v o c . toGraph

    -- | Convert a graph to 'AM.AdjacencyMap'.
    --
    -- @
    -- toAdjacencyMap == 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' 'AM.connect'
    -- @
    toAdjacencyMap :: Ord (ToVertex t) => t -> AM.AdjacencyMap (ToVertex t)
    toAdjacencyMap = foldg AM.empty AM.vertex AM.overlay AM.connect

    -- | Transpose and convert a graph to 'AM.AdjacencyMap'.
    --
    -- @
    -- toAdjacencyMapTranspose == 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' (flip 'AM.connect')
    -- @
    toAdjacencyMapTranspose :: Ord (ToVertex t) => t -> AM.AdjacencyMap (ToVertex t)
    toAdjacencyMapTranspose = foldg AM.empty AM.vertex AM.overlay (flip AM.connect)

    -- | Check if a graph is empty.
    --
    -- @
    -- isEmpty == 'foldg' True (const False) (&&) (&&)
    -- @
    isEmpty :: t -> Bool
    isEmpty = foldg True (const False) (&&) (&&)

    -- | The /size/ of a graph, i.e. the number of leaves of the expression
    -- including 'empty' leaves.
    --
    -- @
    -- size == 'foldg' 1 (const 1) (+) (+)
    -- @
    size :: t -> Int
    size = foldg 1 (const 1) (+) (+)

    -- | Check if a graph contains a given vertex.
    --
    -- @
    -- hasVertex x == 'foldg' False (==x) (||) (||)
    -- @
    hasVertex :: Eq (ToVertex t) => ToVertex t -> t -> Bool
    hasVertex x = foldg False (==x) (||) (||)

    -- | Check if a graph contains a given edge.
    --
    -- @
    -- hasEdge x y == 'elem' (x,y) . 'edgeList'
    -- @
    hasEdge :: Eq (ToVertex t) => ToVertex t -> ToVertex t -> t -> Bool
    hasEdge s t g = case foldg e v o c g of (_, _, r) -> r
      where
        e                             = (False   , False   , False                 )
        v x                           = (x == s  , x == t  , False                 )
        o (xs, xt, xst) (ys, yt, yst) = (xs || ys, xt || yt,             xst || yst)
        c (xs, xt, xst) (ys, yt, yst) = (xs || ys, xt || yt, xs && yt || xst || yst)

    -- | The number of vertices in a graph.
    --
    -- @
    -- vertexCount == Set.'Set.size' . 'vertexSet'
    -- @
    vertexCount :: Ord (ToVertex t) => t -> Int
    vertexCount = Set.size . vertexSet

    -- | The sorted list of vertices of a given graph.
    --
    -- @
    -- vertexList == Set.'Set.toAscList' . 'vertexSet'
    -- @
    vertexList :: Ord (ToVertex t) => t -> [ToVertex t]
    vertexList = Set.toAscList . vertexSet

    -- | The set of vertices of a given graph.
    --
    -- @
    -- vertexSet == 'foldg' Set.'Set.empty' Set.'Set.singleton' Set.'Set.union' Set.'Set.union'
    -- @
    vertexSet :: Ord (ToVertex t) => t -> Set.Set (ToVertex t)
    vertexSet = foldg Set.empty Set.singleton Set.union Set.union

    -- | The set of vertices of a given graph. Like 'vertexSet' but specialised
    -- for graphs with vertices of type 'Int'.
    --
    -- @
    -- vertexIntSet == 'foldg' IntSet.'IntSet.empty' IntSet.'IntSet.singleton' IntSet.'IntSet.union' IntSet.'IntSet.union'
    -- @
    vertexIntSet :: ToVertex t ~ Int => t -> IntSet.IntSet
    vertexIntSet = foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union

    -- | The number of edges in a graph.
    --
    -- @
    -- edgeCount == 'length' . 'edgeList'
    -- @
    edgeCount :: Ord (ToVertex t) => t -> Int
    edgeCount = length . edgeList

    -- | The sorted list of edges of a graph.
    --
    -- @
    -- edgeList == 'AM.edgeList' . 'toAdjacencyMap'
    -- @
    edgeList :: Ord (ToVertex t) => t -> [(ToVertex t, ToVertex t)]
    edgeList = AM.edgeList . toAdjacencyMap

    -- | The set of edges of a given graph.
    -- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
    --
    -- @
    -- edgeSet == 'AM.edgeSet' . 'toAdjacencyMap'
    -- @
    edgeSet :: Ord (ToVertex t) => t -> Set.Set (ToVertex t, ToVertex t)
    edgeSet = AM.edgeSet . toAdjacencyMap

    -- | The sorted /adjacency list/ of a graph.
    --
    -- @
    -- adjacencyList == 'AM.adjacencyList' . 'toAdjacencyMap'
    -- @
    adjacencyList :: Ord (ToVertex t) => t -> [(ToVertex t, [ToVertex t])]
    adjacencyList = AM.adjacencyList . toAdjacencyMap

    -- | The /preset/ (here 'preSet') of an element @x@ is the set of its
    -- /direct predecessors/.
    --
    -- @
    -- preSet x == 'AM.preSet' x . 'toAdjacencyMap'
    -- @
    preSet :: Ord (ToVertex t) => ToVertex t -> t -> Set.Set (ToVertex t)
    preSet x = AM.preSet x . toAdjacencyMap

    -- | The /postset/ (here 'postIntSet') of a vertex is the set of its
    -- /direct successors/. List 'preSet' but specialised for graphs with
    -- vertices of type 'Int'.
    --
    -- @
    -- preIntSet x == IntSet.'IntSet.fromAscList' . Set.'Set.toAscList' . 'preSet' x
    -- @
    preIntSet :: ToVertex t ~ Int => Int -> t -> IntSet.IntSet
    preIntSet x = IntSet.fromAscList . Set.toAscList . preSet x

    -- | The /postset/ (here 'postSet') of a vertex is the set of its
    -- /direct successors/.
    --
    -- @
    -- postSet x == 'AM.preSet' x . 'toAdjacencyMapTranspose'
    -- @
    postSet :: Ord (ToVertex t) => ToVertex t -> t -> Set.Set (ToVertex t)
    postSet x = AM.preSet x . toAdjacencyMapTranspose

    -- | The /postset/ (here 'postIntSet') of a vertex is the set of its
    -- /direct successors/. List 'postSet' but specialised for graphs with
    -- vertices of type 'Int'.
    --
    -- @
    -- postIntSet x == IntSet.'IntSet.fromAscList' . Set.'Set.toAscList' . 'postSet' x
    -- @
    postIntSet :: ToVertex t ~ Int => Int -> t -> IntSet.IntSet
    postIntSet x = IntSet.fromAscList . Set.toAscList . postSet x

instance ToGraph (G.Graph a) where
    type ToVertex (G.Graph a) = a
    toGraph = id
    foldg   = G.foldg

instance Ord a => ToGraph (AM.AdjacencyMap a) where
    type ToVertex (AM.AdjacencyMap a) = a
    toGraph        = G.fromAdjacencyList
                   . map (fmap Set.toList)
                   . Map.toList
                   . AM.adjacencyMap
    toAdjacencyMap = id
    isEmpty        = AM.isEmpty
    hasVertex      = AM.hasVertex
    hasEdge        = AM.hasEdge
    vertexCount    = AM.vertexCount
    edgeCount      = AM.edgeCount
    vertexList     = AM.vertexList
    vertexSet      = AM.vertexSet
    vertexIntSet   = AM.vertexIntSet
    edgeList       = AM.edgeList
    edgeSet        = AM.edgeSet
    adjacencyList  = AM.adjacencyList
    preSet         = AM.preSet
    postSet        = AM.postSet

instance ToGraph IAM.IntAdjacencyMap where
    type ToVertex IAM.IntAdjacencyMap = Int
    toGraph        = G.overlays
                   . map (uncurry G.star . fmap IntSet.toList)
                   . IntMap.toList
                   . IAM.adjacencyMap
    toAdjacencyMap = AM.AM
                   . Map.fromAscList
                   . map (fmap $ Set.fromAscList . IntSet.toAscList)
                   . IntMap.toAscList
                   . IAM.adjacencyMap
    isEmpty        = IAM.isEmpty
    hasVertex      = IAM.hasVertex
    hasEdge        = IAM.hasEdge
    vertexCount    = IAM.vertexCount
    edgeCount      = IAM.edgeCount
    vertexList     = IAM.vertexList
    vertexSet      = Set.fromAscList . IntSet.toAscList . IAM.vertexIntSet
    vertexIntSet   = IAM.vertexIntSet
    edgeList       = IAM.edgeList
    edgeSet        = IAM.edgeSet
    adjacencyList  = IAM.adjacencyList
    preIntSet      = IAM.preIntSet
    postIntSet     = IAM.postIntSet

instance Ord a => ToGraph (R.Relation a) where
    type ToVertex (R.Relation a) = a
    toGraph r      = G.vertices (Set.toList $ R.domain   r) `G.overlay`
                     G.edges    (Set.toList $ R.relation r)
    toAdjacencyMap = AM.AM
                   . Map.fromAscList
                   . map (fmap Set.fromAscList)
                   . R.adjacencyList
    isEmpty        = R.isEmpty
    hasVertex      = R.hasVertex
    hasEdge        = R.hasEdge
    vertexCount    = R.vertexCount
    edgeCount      = R.edgeCount
    vertexList     = R.vertexList
    vertexSet      = R.vertexSet
    vertexIntSet   = R.vertexIntSet
    edgeList       = R.edgeList
    edgeSet        = R.edgeSet
    adjacencyList  = R.adjacencyList
