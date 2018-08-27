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
import Data.Tree

import qualified Algebra.Graph                          as G
import qualified Algebra.Graph.AdjacencyMap             as AM
import qualified Algebra.Graph.AdjacencyMap.Internal    as AM
import qualified Algebra.Graph.AdjacencyIntMap          as AIM
import qualified Algebra.Graph.AdjacencyIntMap.Internal as AIM
import qualified Algebra.Graph.Relation                 as R
import qualified Data.IntMap                            as IntMap
import qualified Data.IntSet                            as IntSet
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set

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
    -- hasEdge x y == Algebra.Graph.'G.hasEdge' x y . 'toGraph'
    -- @
    hasEdge :: Eq (ToVertex t) => ToVertex t -> ToVertex t -> t -> Bool
    hasEdge x y = G.hasEdge x y . toGraph

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
    -- edgeCount == Set.'Set.size' . 'edgeSet'
    -- @
    edgeCount :: Ord (ToVertex t) => t -> Int
    edgeCount = AM.edgeCount . toAdjacencyMap

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
    -- edgeList == Set.'Set.toAscList' . 'edgeSet'
    -- @
    edgeList :: Ord (ToVertex t) => t -> [(ToVertex t, ToVertex t)]
    edgeList = AM.edgeList . toAdjacencyMap

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
    -- edgeSet == Algebra.Graph.AdjacencyMap.'AM.edgeSet' . 'toAdjacencyMap'
    -- @
    edgeSet :: Ord (ToVertex t) => t -> Set (ToVertex t, ToVertex t)
    edgeSet = AM.edgeSet . toAdjacencyMap

    -- | The /preset/ of a vertex is the set of its /direct predecessors/.
    --
    -- @
    -- preSet x == Algebra.Graph.AdjacencyMap.'AM.preSet' x . 'toAdjacencyMap'
    -- @
    preSet :: Ord (ToVertex t) => ToVertex t -> t -> Set (ToVertex t)
    preSet x = AM.postSet x . toAdjacencyMapTranspose

    -- | The /preset/ (here @preIntSet@) of a vertex is the set of its
    -- /direct predecessors/. Like 'preSet' but specialised for graphs with
    -- vertices of type 'Int'.
    --
    -- @
    -- preIntSet x == Algebra.Graph.AdjacencyIntMap.'AIM.preIntSet' x . 'toAdjacencyIntMap'
    -- @
    preIntSet :: ToVertex t ~ Int => Int -> t -> IntSet
    preIntSet x = AIM.postIntSet x . toAdjacencyIntMapTranspose

    -- | The /postset/ of a vertex is the set of its /direct successors/.
    --
    -- @
    -- postSet x == Algebra.Graph.AdjacencyMap.'AM.postSet' x . 'toAdjacencyMap'
    -- @
    postSet :: Ord (ToVertex t) => ToVertex t -> t -> Set (ToVertex t)
    postSet x = AM.postSet x . toAdjacencyMap

    -- | The /postset/ (here @postIntSet@) of a vertex is the set of its
    -- /direct successors/. Like 'postSet' but specialised for graphs with
    -- vertices of type 'Int'.
    --
    -- @
    -- postIntSet x == Algebra.Graph.AdjacencyIntMap.'AIM.postIntSet' x . 'toAdjacencyIntMap'
    -- @
    postIntSet :: ToVertex t ~ Int => Int -> t -> IntSet
    postIntSet x = AIM.postIntSet x . toAdjacencyIntMap

    -- | The sorted /adjacency list/ of a graph.
    --
    -- @
    -- adjacencyList == Algebra.Graph.AdjacencyMap.'AM.adjacencyList' . 'toAdjacencyMap'
    -- @
    adjacencyList :: Ord (ToVertex t) => t -> [(ToVertex t, [ToVertex t])]
    adjacencyList = AM.adjacencyList . toAdjacencyMap

    -- | The /adjacency map/ of a graph: each vertex is associated with a set
    -- of its /direct successors/.
    --
    -- @
    -- adjacencyMap == Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.adjacencyMap' . 'toAdjacencyMap'
    -- @
    adjacencyMap :: Ord (ToVertex t) => t -> Map (ToVertex t) (Set (ToVertex t))
    adjacencyMap = AM.adjacencyMap . toAdjacencyMap

    -- | The /adjacency map/ of a graph: each vertex is associated with a set
    -- of its /direct successors/. Like 'adjacencyMap' but specialised for
    -- graphs with vertices of type 'Int'.
    --
    -- @
    -- adjacencyIntMap == Algebra.Graph.AdjacencyIntMap.'Algebra.Graph.AdjacencyIntMap.adjacencyIntMap' . 'toAdjacencyIntMap'
    -- @
    adjacencyIntMap :: ToVertex t ~ Int => t -> IntMap IntSet
    adjacencyIntMap = AIM.adjacencyIntMap . toAdjacencyIntMap

    -- | The transposed /adjacency map/ of a graph: each vertex is associated
    -- with a set of its /direct predecessors/.
    --
    -- @
    -- adjacencyMapTranspose == Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.adjacencyMap' . 'toAdjacencyMapTranspose'
    -- @
    adjacencyMapTranspose :: Ord (ToVertex t) => t -> Map (ToVertex t) (Set (ToVertex t))
    adjacencyMapTranspose = AM.adjacencyMap . toAdjacencyMapTranspose

    -- | The transposed /adjacency map/ of a graph: each vertex is associated
    -- with a set of its /direct predecessors/. Like 'adjacencyMapTranspose' but
    -- specialised for graphs with vertices of type 'Int'.
    --
    -- @
    -- adjacencyIntMapTranspose == Algebra.Graph.AdjacencyIntMap.'Algebra.Graph.AdjacencyIntMap.adjacencyIntMap' . 'toAdjacencyIntMapTranspose'
    -- @
    adjacencyIntMapTranspose :: ToVertex t ~ Int => t -> IntMap IntSet
    adjacencyIntMapTranspose = AIM.adjacencyIntMap . toAdjacencyIntMapTranspose

    -- | Compute the /depth-first search/ forest of a graph.
    --
    -- @
    -- dfsForest == Algebra.Graph.AdjacencyMap.'AM.dfsForest' . toAdjacencyMap
    -- @
    dfsForest :: Ord (ToVertex t) => t -> Forest (ToVertex t)
    dfsForest = AM.dfsForest . toAdjacencyMap

    -- | Compute the /depth-first search/ forest of a graph, searching from each
    -- of the given vertices in order. Note that the resulting forest does not
    -- necessarily span the whole graph, as some vertices may be unreachable.
    --
    -- @
    -- dfsForestFrom vs == Algebra.Graph.AdjacencyMap.'AM.dfsForestFrom' vs . toAdjacencyMap
    -- @
    dfsForestFrom :: Ord (ToVertex t) => [ToVertex t] -> t -> Forest (ToVertex t)
    dfsForestFrom vs = AM.dfsForestFrom vs . toAdjacencyMap

    -- | Compute the list of vertices visited by the /depth-first search/ in a
    -- graph, when searching from each of the given vertices in order.
    --
    -- @
    -- dfs vs == Algebra.Graph.AdjacencyMap.'AM.dfs' vs . toAdjacencyMap
    -- @
    dfs :: Ord (ToVertex t) => [ToVertex t] -> t -> [ToVertex t]
    dfs vs = AM.dfs vs . toAdjacencyMap

    -- | Compute the list of vertices that are /reachable/ from a given source
    -- vertex in a graph. The vertices in the resulting list appear in the
    -- /depth-first order/.
    --
    -- @
    -- reachable x == Algebra.Graph.AdjacencyMap.'AM.reachable' x . toAdjacencyMap
    -- @
    reachable :: Ord (ToVertex t) => ToVertex t -> t -> [ToVertex t]
    reachable x = AM.reachable x . toAdjacencyMap

    -- | Compute the /topological sort/ of a graph or return @Nothing@ if the
    -- graph is cyclic.
    --
    -- @
    -- topSort == Algebra.Graph.AdjacencyMap.'AM.topSort' . toAdjacencyMap
    -- @
    topSort :: Ord (ToVertex t) => t -> Maybe [ToVertex t]
    topSort = AM.topSort . toAdjacencyMap

    -- | Check if a given graph is /acyclic/.
    --
    -- @
    -- isAcyclic == Algebra.Graph.AdjacencyMap.'AM.isAcyclic' . toAdjacencyMap
    -- @
    isAcyclic :: Ord (ToVertex t) => t -> Bool
    isAcyclic = AM.isAcyclic . toAdjacencyMap

    -- | Check if a given list of vertices is a valid /topological sort/ of a
    -- graph.
    --
    -- @
    -- isTopSortOf vs == Algebra.Graph.AdjacencyMap.'AM.isTopSortOf' vs . toAdjacencyMap
    -- @
    isTopSortOf :: Ord (ToVertex t) => [ToVertex t] -> t -> Bool
    isTopSortOf vs = AM.isTopSortOf vs . toAdjacencyMap

    -- | Convert a value to the corresponding 'AM.AdjacencyMap'.
    --
    -- @
    -- toAdjacencyMap == 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' 'AM.connect'
    -- @
    toAdjacencyMap :: Ord (ToVertex t) => t -> AM.AdjacencyMap (ToVertex t)
    toAdjacencyMap = foldg AM.empty AM.vertex AM.overlay AM.connect

    -- | Convert a value to the corresponding 'AM.AdjacencyMap' and transpose the
    -- result.
    --
    -- @
    -- toAdjacencyMapTranspose == 'foldg' 'AM.empty' 'AM.vertex' 'AM.overlay' (flip 'AM.connect')
    -- @
    toAdjacencyMapTranspose :: Ord (ToVertex t) => t -> AM.AdjacencyMap (ToVertex t)
    toAdjacencyMapTranspose = foldg AM.empty AM.vertex AM.overlay (flip AM.connect)

    -- | Convert a value to the corresponding 'AIM.AdjacencyIntMap'.
    --
    -- @
    -- toAdjacencyIntMap == 'foldg' 'AIM.empty' 'AIM.vertex' 'AIM.overlay' 'AIM.connect'
    -- @
    toAdjacencyIntMap :: ToVertex t ~ Int => t -> AIM.AdjacencyIntMap
    toAdjacencyIntMap = foldg AIM.empty AIM.vertex AIM.overlay AIM.connect

    -- | Convert a value to the corresponding 'AIM.AdjacencyIntMap' and transpose
    -- the result.
    --
    -- @
    -- toAdjacencyIntMapTranspose == 'foldg' 'AIM.empty' 'AIM.vertex' 'AIM.overlay' (flip 'AIM.connect')
    -- @
    toAdjacencyIntMapTranspose :: ToVertex t ~ Int => t -> AIM.AdjacencyIntMap
    toAdjacencyIntMapTranspose = foldg AIM.empty AIM.vertex AIM.overlay (flip AIM.connect)

instance Ord a => ToGraph (G.Graph a) where
    type ToVertex (G.Graph a) = a
    toGraph = id
    foldg   = G.foldg
    hasEdge = G.hasEdge

instance Ord a => ToGraph (AM.AdjacencyMap a) where
    type ToVertex (AM.AdjacencyMap a) = a
    toGraph                    = G.stars
                               . map (fmap Set.toList)
                               . Map.toList
                               . AM.adjacencyMap
    isEmpty                    = AM.isEmpty
    hasVertex                  = AM.hasVertex
    hasEdge                    = AM.hasEdge
    vertexCount                = AM.vertexCount
    edgeCount                  = AM.edgeCount
    vertexList                 = AM.vertexList
    vertexSet                  = AM.vertexSet
    vertexIntSet               = AM.vertexIntSet
    edgeList                   = AM.edgeList
    edgeSet                    = AM.edgeSet
    adjacencyList              = AM.adjacencyList
    preSet                     = AM.preSet
    postSet                    = AM.postSet
    adjacencyMap               = AM.adjacencyMap
    adjacencyIntMap            = IntMap.fromAscList
                               . map (fmap $ IntSet.fromAscList . Set.toAscList)
                               . Map.toAscList
                               . AM.adjacencyMap
    toAdjacencyMap             = id
    toAdjacencyIntMap          = AIM.AM . adjacencyIntMap
    toAdjacencyMapTranspose    = AM.transpose . toAdjacencyMap
    toAdjacencyIntMapTranspose = AIM.transpose . toAdjacencyIntMap

instance ToGraph AIM.AdjacencyIntMap where
    type ToVertex AIM.AdjacencyIntMap = Int
    toGraph                    = G.stars
                               . map (fmap IntSet.toList)
                               . IntMap.toList
                               . AIM.adjacencyIntMap
    isEmpty                    = AIM.isEmpty
    hasVertex                  = AIM.hasVertex
    hasEdge                    = AIM.hasEdge
    vertexCount                = AIM.vertexCount
    edgeCount                  = AIM.edgeCount
    vertexList                 = AIM.vertexList
    vertexSet                  = Set.fromAscList . IntSet.toAscList . AIM.vertexIntSet
    vertexIntSet               = AIM.vertexIntSet
    edgeList                   = AIM.edgeList
    edgeSet                    = AIM.edgeSet
    adjacencyList              = AIM.adjacencyList
    preIntSet                  = AIM.preIntSet
    postIntSet                 = AIM.postIntSet
    adjacencyMap               = Map.fromAscList
                               . map (fmap $ Set.fromAscList . IntSet.toAscList)
                               . IntMap.toAscList
                               . AIM.adjacencyIntMap
    adjacencyIntMap            = AIM.adjacencyIntMap
    toAdjacencyMap             = AM.AM . adjacencyMap
    toAdjacencyIntMap          = id
    toAdjacencyMapTranspose    = AM.transpose . toAdjacencyMap
    toAdjacencyIntMapTranspose = AIM.transpose . toAdjacencyIntMap

-- TODO: Get rid of "Relation.Internal" and move this instance to "Relation".
instance Ord a => ToGraph (R.Relation a) where
    type ToVertex (R.Relation a) = a
    toGraph r                  = G.vertices (Set.toList $ R.domain   r) `G.overlay`
                                 G.edges    (Set.toList $ R.relation r)
    isEmpty                    = R.isEmpty
    hasVertex                  = R.hasVertex
    hasEdge                    = R.hasEdge
    vertexCount                = R.vertexCount
    edgeCount                  = R.edgeCount
    vertexList                 = R.vertexList
    vertexSet                  = R.vertexSet
    vertexIntSet               = R.vertexIntSet
    edgeList                   = R.edgeList
    edgeSet                    = R.edgeSet
    adjacencyList              = R.adjacencyList
    adjacencyMap               = Map.fromAscList
                               . map (fmap Set.fromAscList)
                               . R.adjacencyList
    adjacencyIntMap            = IntMap.fromAscList
                               . map (fmap IntSet.fromAscList)
                               . R.adjacencyList
    toAdjacencyMap             = AM.AM . adjacencyMap
    toAdjacencyIntMap          = AIM.AM . adjacencyIntMap
    toAdjacencyMapTranspose    = AM.transpose . toAdjacencyMap
    toAdjacencyIntMapTranspose = AIM.transpose . toAdjacencyIntMap
