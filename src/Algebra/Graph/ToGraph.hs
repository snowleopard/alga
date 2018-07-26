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
-- This module defines the type class 'ToGraph' for capturing data types that
-- can be converted to algebraic graphs. To make an instance of this class you
-- need to define just a single method ('toGraph' or 'foldg'), which gives you
-- access to many other useful methods for free. This type class is similar to
-- the standard "Data.Foldable" defined for lists.
--
-----------------------------------------------------------------------------
module Algebra.Graph.ToGraph (ToGraph (..)) where

import Prelude ()
import Prelude.Compat

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map    (Map)
import Data.Set    (Set)

import qualified Algebra.Graph                 as G
import qualified Algebra.Graph.AdjacencyMap    as AM
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import qualified Algebra.Graph.Relation        as R
import qualified Data.IntMap                   as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | The 'ToGraph' type class captures data types that can be converted to
-- algebraic graphs.
class ToGraph t where
    {-# MINIMAL toGraph | foldg #-}
    type ToVertex t

    -- | Convert a value to the corresponding algebraic graph, see "Algebra.Graph".
    --
    -- @
    -- toGraph == 'foldg' 'G.Empty' 'G.Vertex' 'G.Overlay' 'G.Connect'
    -- @
    toGraph :: t -> G.Graph (ToVertex t)
    toGraph = foldg G.Empty G.Vertex G.Overlay G.Connect

    -- | The method 'foldg' is used for generalised graph folding. It collapses
    -- a given value by applying the provided graph construction primitives. The
    -- order of arguments is: empty, vertex, overlay and connect, and it is
    -- assumed that the arguments satisfy the axioms of the graph algebra.
    --
    -- @
    -- foldg == Algebra.Graph.'G.foldg' . 'toGraph'
    -- @
    foldg :: r -> (ToVertex t -> r) -> (r -> r -> r) -> (r -> r -> r) -> t -> r
    foldg e v o c = G.foldg e v o c . toGraph

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
    hasEdge s t = G.hasEdge s t . toGraph

    -- | The number of vertices in a graph.
    --
    -- @
    -- vertexCount == Set.'Set.size' . 'vertexSet'
    -- @
    vertexCount :: Ord (ToVertex t) => t -> Int
    vertexCount = Set.size . vertexSet

    -- | The number of edges in a graph.
    --
    -- @
    -- edgeCount == 'length' . 'edgeList'
    -- @
    edgeCount :: Ord (ToVertex t) => t -> Int
    edgeCount = length . edgeList

    -- | The sorted list of vertices of a given graph.
    --
    -- @
    -- vertexList == Set.'Set.toAscList' . 'vertexSet'
    -- @
    vertexList :: Ord (ToVertex t) => t -> [ToVertex t]
    vertexList = Set.toAscList . vertexSet

    -- | The sorted list of edges of a graph.
    --
    -- @
    -- edgeList == Algebra.Graph.AdjacencyMap.'AM.edgeList' . 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' 'AM.connect'
    -- @
    edgeList :: Ord (ToVertex t) => t -> [(ToVertex t, ToVertex t)]
    edgeList = AM.edgeList . foldg AM.empty AM.vertex AM.overlay AM.connect

    -- | The set of vertices of a graph.
    --
    -- @
    -- vertexSet == 'foldg' Set.'Set.empty' Set.'Set.singleton' Set.'Set.union' Set.'Set.union'
    -- @
    vertexSet :: Ord (ToVertex t) => t -> Set (ToVertex t)
    vertexSet = foldg Set.empty Set.singleton Set.union Set.union

    -- | The set of vertices of a graph. Like 'vertexSet' but specialised for
    -- graphs with vertices of type 'Int'.
    --
    -- @
    -- vertexIntSet == 'foldg' IntSet.'IntSet.empty' IntSet.'IntSet.singleton' IntSet.'IntSet.union' IntSet.'IntSet.union'
    -- @
    vertexIntSet :: ToVertex t ~ Int => t -> IntSet
    vertexIntSet = foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union

    -- | The set of edges of a graph.
    --
    -- @
    -- edgeSet == Algebra.Graph.AdjacencyMap.'AM.edgeSet' . 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' 'AM.connect'
    -- @
    edgeSet :: Ord (ToVertex t) => t -> Set (ToVertex t, ToVertex t)
    edgeSet = AM.edgeSet . foldg AM.empty AM.vertex AM.overlay AM.connect

    -- | The /preset/ (here 'preSet') of a vertex is the set of its
    -- /direct predecessors/.
    --
    -- @
    -- preSet x == Algebra.Graph.AdjacencyMap.'AM.postSet' x . 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' (flip 'AM.connect')
    -- @
    preSet :: Ord (ToVertex t) => ToVertex t -> t -> Set (ToVertex t)
    preSet x = AM.postSet x . foldg AM.empty AM.vertex AM.overlay (flip AM.connect)

    -- | The /preset/ (here 'preIntSet') of a vertex is the set of its
    -- /direct predecessors/. List 'preSet' but specialised for graphs with
    -- vertices of type 'Int'.
    --
    -- @
    -- preIntSet x == IntSet.'IntSet.fromAscList' . Set.'Set.toAscList' . 'preSet' x
    -- @
    preIntSet :: ToVertex t ~ Int => Int -> t -> IntSet
    preIntSet x = IntSet.fromAscList . Set.toAscList . preSet x

    -- | The /postset/ (here 'postSet') of a vertex is the set of its
    -- /direct successors/.
    --
    -- @
    -- postSet x == Algebra.Graph.AdjacencyMap.'AM.postSet' x . 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' 'AM.connect'
    -- @
    postSet :: Ord (ToVertex t) => ToVertex t -> t -> Set (ToVertex t)
    postSet x = AM.postSet x . foldg AM.empty AM.vertex AM.overlay AM.connect

    -- | The /postset/ (here 'postIntSet') of a vertex is the set of its
    -- /direct successors/. List 'postSet' but specialised for graphs with
    -- vertices of type 'Int'.
    --
    -- @
    -- postIntSet x == IntSet.'IntSet.fromAscList' . Set.'Set.toAscList' . 'postSet' x
    -- @
    postIntSet :: ToVertex t ~ Int => Int -> t -> IntSet
    postIntSet x = IntSet.fromAscList . Set.toAscList . postSet x

    -- | The sorted /adjacency list/ of a graph.
    --
    -- @
    -- adjacencyList == Algebra.Graph.AdjacencyMap.'AM.adjacencyList' . 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' 'AM.connect'
    -- @
    adjacencyList :: Ord (ToVertex t) => t -> [(ToVertex t, [ToVertex t])]
    adjacencyList = AM.adjacencyList . foldg AM.empty AM.vertex AM.overlay AM.connect

    -- | The /adjacency map/ of a graph: each vertex is associated with a set
    -- of its /direct successors/.
    --
    -- @
    -- adjacencyMap == Algebra.Graph.AdjacencyMap.'AM.adjacencyMap' . 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' 'AM.connect'
    -- @
    adjacencyMap :: Ord (ToVertex t) => t -> Map (ToVertex t) (Set (ToVertex t))
    adjacencyMap = AM.adjacencyMap . foldg AM.empty AM.vertex AM.overlay AM.connect

    -- | The /adjacency map/ of a graph: each vertex is associated with a set
    -- of its /direct successors/. List 'adjacencyMap' but specialised for
    -- graphs with vertices of type 'Int'.
    --
    -- @
    -- adjacencyIntMap == Algebra.Graph.AdjacencyIntMap.'AIM.adjacencyIntMap' . 'foldg' 'AIM.empty' 'AIM.vertex' 'AIM.overlay' 'AIM.connect'
    -- @
    adjacencyIntMap :: ToVertex t ~ Int => t -> IntMap IntSet
    adjacencyIntMap = AIM.adjacencyIntMap . foldg AIM.empty AIM.vertex AIM.overlay AIM.connect

    -- | The transposed /adjacency map/ of a graph: each vertex is associated
    -- with a set of its /direct predecessors/.
    --
    -- @
    -- adjacencyMapTranspose == Algebra.Graph.AdjacencyMap.'AM.adjacencyMap' . 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' (flip 'AM.connect')
    -- @
    adjacencyMapTranspose :: Ord (ToVertex t) => t -> Map (ToVertex t) (Set (ToVertex t))
    adjacencyMapTranspose = AM.adjacencyMap . foldg AM.empty AM.vertex AM.overlay (flip AM.connect)

    -- | The transposed /adjacency map/ of a graph: each vertex is associated
    -- with a set of its /direct predecessors/. List 'adjacencyMapTranspose' but
    -- specialised for graphs with vertices of type 'Int'.
    --
    -- @
    -- adjacencyIntMapTranspose == Algebra.Graph.AdjacencyIntMap.'AIM.adjacencyIntMap' . 'foldg' 'AIM.empty' 'AIM.vertex' 'AIM.overlay' (flip 'AIM.connect')
    -- @
    adjacencyIntMapTranspose :: ToVertex t ~ Int => t -> IntMap IntSet
    adjacencyIntMapTranspose = AIM.adjacencyIntMap . foldg AIM.empty AIM.vertex AIM.overlay (flip AIM.connect)

instance Ord a => ToGraph (G.Graph a) where
    type ToVertex (G.Graph a) = a
    toGraph         = id
    foldg           = G.foldg
    isEmpty         = G.isEmpty
    hasVertex       = G.hasVertex
    hasEdge         = G.hasEdge
    vertexCount     = G.vertexCount
    edgeCount       = G.edgeCount
    vertexList      = G.vertexList
    vertexSet       = G.vertexSet
    vertexIntSet    = G.vertexIntSet
    edgeList        = G.edgeList
    edgeSet         = G.edgeSet
    adjacencyList   = G.adjacencyList
    adjacencyMap    = G.adjacencyMap
    adjacencyIntMap = G.adjacencyIntMap

instance Ord a => ToGraph (AM.AdjacencyMap a) where
    type ToVertex (AM.AdjacencyMap a) = a
    toGraph         = G.fromAdjacencyList
                    . map (fmap Set.toList)
                    . Map.toList
                    . AM.adjacencyMap
    isEmpty         = AM.isEmpty
    hasVertex       = AM.hasVertex
    hasEdge         = AM.hasEdge
    vertexCount     = AM.vertexCount
    edgeCount       = AM.edgeCount
    vertexList      = AM.vertexList
    vertexSet       = AM.vertexSet
    vertexIntSet    = AM.vertexIntSet
    edgeList        = AM.edgeList
    edgeSet         = AM.edgeSet
    adjacencyList   = AM.adjacencyList
    preSet          = AM.preSet
    postSet         = AM.postSet
    adjacencyMap    = AM.adjacencyMap
    adjacencyIntMap = IntMap.fromAscList
                    . map (fmap $ IntSet.fromAscList . Set.toAscList)
                    . Map.toAscList
                    . AM.adjacencyMap

instance ToGraph AIM.AdjacencyIntMap where
    type ToVertex AIM.AdjacencyIntMap = Int
    toGraph         = G.overlays
                    . map (uncurry G.star . fmap IntSet.toList)
                    . IntMap.toList
                    . AIM.adjacencyIntMap
    isEmpty         = AIM.isEmpty
    hasVertex       = AIM.hasVertex
    hasEdge         = AIM.hasEdge
    vertexCount     = AIM.vertexCount
    edgeCount       = AIM.edgeCount
    vertexList      = AIM.vertexList
    vertexSet       = Set.fromAscList . IntSet.toAscList . AIM.vertexIntSet
    vertexIntSet    = AIM.vertexIntSet
    edgeList        = AIM.edgeList
    edgeSet         = AIM.edgeSet
    adjacencyList   = AIM.adjacencyList
    preIntSet       = AIM.preIntSet
    postIntSet      = AIM.postIntSet
    adjacencyMap    = Map.fromAscList
                    . map (fmap $ Set.fromAscList . IntSet.toAscList)
                    . IntMap.toAscList
                    . AIM.adjacencyIntMap
    adjacencyIntMap = AIM.adjacencyIntMap

instance Ord a => ToGraph (R.Relation a) where
    type ToVertex (R.Relation a) = a
    toGraph r       = G.vertices (Set.toList $ R.domain   r) `G.overlay`
                      G.edges    (Set.toList $ R.relation r)
    isEmpty         = R.isEmpty
    hasVertex       = R.hasVertex
    hasEdge         = R.hasEdge
    vertexCount     = R.vertexCount
    edgeCount       = R.edgeCount
    vertexList      = R.vertexList
    vertexSet       = R.vertexSet
    vertexIntSet    = R.vertexIntSet
    edgeList        = R.edgeList
    edgeSet         = R.edgeSet
    adjacencyList   = R.adjacencyList
    adjacencyMap    = Map.fromAscList
                    . map (fmap Set.fromAscList)
                    . R.adjacencyList
    adjacencyIntMap = IntMap.fromAscList
                    . map (fmap IntSet.fromAscList)
                    . R.adjacencyList
