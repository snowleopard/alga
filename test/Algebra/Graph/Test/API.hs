{-# LANGUAGE ConstrainedClassMethods #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.API
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Graph manipulation API used for generic testing.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.API (
    -- * Graph manipulation API
    GraphAPI (..)
  ) where

import Data.IntSet (IntSet)
import Data.Set (Set)
import Data.Tree

import Algebra.Graph.Class

import qualified Algebra.Graph.AdjacencyMap    as AdjacencyMap
import qualified Algebra.Graph.Fold            as Fold
import qualified Algebra.Graph                 as Graph
import qualified Algebra.Graph.IntAdjacencyMap as IntAdjacencyMap
import qualified Algebra.Graph.Relation        as Relation

class Graph g => GraphAPI g where
    edge          :: Vertex g -> Vertex g -> g
    edge          = notImplemented
    vertices      :: [Vertex g] -> g
    vertices      = notImplemented
    edges         :: [(Vertex g, Vertex g)] -> g
    edges         = notImplemented
    overlays      :: [g] -> g
    overlays      = notImplemented
    connects      :: [g] -> g
    connects      = notImplemented
    graph         :: [Vertex g] -> [(Vertex g, Vertex g)] -> g
    graph         = notImplemented
    foldg         :: r -> (Vertex g -> r) -> (r -> r -> r) -> (r -> r -> r) -> g -> r
    foldg         = notImplemented
    isSubgraphOf  :: g -> g -> Bool
    isSubgraphOf  = notImplemented
    (===)         :: g -> g -> Bool
    (===)         = notImplemented
    isEmpty       :: g -> Bool
    isEmpty       = notImplemented
    size          :: g -> Int
    size          = notImplemented
    hasVertex     :: Vertex g -> g -> Bool
    hasVertex     = notImplemented
    hasEdge       :: Vertex g -> Vertex g -> g -> Bool
    hasEdge       = notImplemented
    vertexCount   :: g -> Int
    vertexCount   = notImplemented
    edgeCount     :: g -> Int
    edgeCount     = notImplemented
    vertexList    :: g -> [Vertex g]
    vertexList    = notImplemented
    edgeList      :: g -> [(Vertex g, Vertex g)]
    edgeList      = notImplemented
    vertexSet     :: g -> Set (Vertex g)
    vertexSet     = notImplemented
    vertexIntSet  :: Vertex g ~ Int => g -> IntSet
    vertexIntSet  = notImplemented
    edgeSet       :: g -> Set (Vertex g, Vertex g)
    edgeSet       = notImplemented
    path          :: [Vertex g] -> g
    path          = notImplemented
    circuit       :: [Vertex g] -> g
    circuit       = notImplemented
    clique        :: [Vertex g] -> g
    clique        = notImplemented
    biclique      :: [Vertex g] -> [Vertex g] -> g
    biclique      = notImplemented
    star          :: Vertex g -> [Vertex g] -> g
    star          = notImplemented
    tree          :: Tree (Vertex g) -> g
    tree          = notImplemented
    forest        :: Forest (Vertex g) -> g
    forest        = notImplemented
    mesh          :: Vertex g ~ (a, b) => [a] -> [b] -> g
    mesh          = notImplemented
    torus         :: Vertex g ~ (a, b) => [a] -> [b] -> g
    torus         = notImplemented
    deBruijn      :: Vertex g ~ [a] => Int -> [a] -> g
    deBruijn      = notImplemented
    removeVertex  :: Vertex g -> g -> g
    removeVertex  = notImplemented
    removeEdge    :: Vertex g -> Vertex g -> g -> g
    removeEdge    = notImplemented
    replaceVertex :: Vertex g -> Vertex g -> g -> g
    replaceVertex = notImplemented
    mergeVertices :: (Vertex g -> Bool) -> Vertex g -> g -> g
    mergeVertices = notImplemented
    splitVertex   :: Vertex g -> [Vertex g] -> g -> g
    splitVertex   = notImplemented
    transpose     :: g -> g
    transpose     = notImplemented
    induce        :: (Vertex g -> Bool) -> g -> g
    induce        = notImplemented
    simplify      :: g -> g
    simplify      = notImplemented
    box           :: forall a b f. g ~ f (a, b) => f a -> f b -> f (a, b)
    box           = notImplemented

notImplemented :: a
notImplemented = error "Not implemented"

instance Ord a => GraphAPI (AdjacencyMap.AdjacencyMap a) where
    edge          = AdjacencyMap.edge
    vertices      = AdjacencyMap.vertices
    edges         = AdjacencyMap.edges
    overlays      = AdjacencyMap.overlays
    connects      = AdjacencyMap.connects
    graph         = AdjacencyMap.graph
    isSubgraphOf  = AdjacencyMap.isSubgraphOf
    isEmpty       = AdjacencyMap.isEmpty
    hasVertex     = AdjacencyMap.hasVertex
    hasEdge       = AdjacencyMap.hasEdge
    vertexCount   = AdjacencyMap.vertexCount
    edgeCount     = AdjacencyMap.edgeCount
    vertexList    = AdjacencyMap.vertexList
    edgeList      = AdjacencyMap.edgeList
    vertexSet     = AdjacencyMap.vertexSet
    edgeSet       = AdjacencyMap.edgeSet
    path          = AdjacencyMap.path
    circuit       = AdjacencyMap.circuit
    clique        = AdjacencyMap.clique
    biclique      = AdjacencyMap.biclique
    star          = AdjacencyMap.star
    tree          = AdjacencyMap.tree
    forest        = AdjacencyMap.forest
    removeVertex  = AdjacencyMap.removeVertex
    removeEdge    = AdjacencyMap.removeEdge
    replaceVertex = AdjacencyMap.replaceVertex
    mergeVertices = AdjacencyMap.mergeVertices
    transpose     = AdjacencyMap.transpose
    induce        = AdjacencyMap.induce

instance Ord a => GraphAPI (Fold.Fold a) where
    edge          = Fold.edge
    vertices      = Fold.vertices
    edges         = Fold.edges
    overlays      = Fold.overlays
    connects      = Fold.connects
    graph         = Fold.graph
    foldg         = Fold.foldg
    isSubgraphOf  = Fold.isSubgraphOf
    isEmpty       = Fold.isEmpty
    size          = Fold.size
    hasVertex     = Fold.hasVertex
    hasEdge       = Fold.hasEdge
    vertexCount   = Fold.vertexCount
    edgeCount     = Fold.edgeCount
    vertexList    = Fold.vertexList
    edgeList      = Fold.edgeList
    vertexSet     = Fold.vertexSet
    vertexIntSet  = Fold.vertexIntSet
    edgeSet       = Fold.edgeSet
    path          = Fold.path
    circuit       = Fold.circuit
    clique        = Fold.clique
    biclique      = Fold.biclique
    star          = Fold.star
    tree          = Fold.tree
    forest        = Fold.forest
    mesh          = Fold.mesh
    torus         = Fold.torus
    deBruijn      = Fold.deBruijn
    removeVertex  = Fold.removeVertex
    removeEdge    = Fold.removeEdge
    replaceVertex = Fold.replaceVertex
    mergeVertices = Fold.mergeVertices
    splitVertex   = Fold.splitVertex
    transpose     = Fold.transpose
    induce        = Fold.induce
    simplify      = Fold.simplify
    box           = Fold.box

instance Ord a => GraphAPI (Graph.Graph a) where
    edge          = Graph.edge
    vertices      = Graph.vertices
    edges         = Graph.edges
    overlays      = Graph.overlays
    connects      = Graph.connects
    graph         = Graph.graph
    foldg         = Graph.foldg
    isSubgraphOf  = Graph.isSubgraphOf
    (===)         = (Graph.===)
    isEmpty       = Graph.isEmpty
    size          = Graph.size
    hasVertex     = Graph.hasVertex
    hasEdge       = Graph.hasEdge
    vertexCount   = Graph.vertexCount
    edgeCount     = Graph.edgeCount
    vertexList    = Graph.vertexList
    edgeList      = Graph.edgeList
    vertexSet     = Graph.vertexSet
    vertexIntSet  = Graph.vertexIntSet
    edgeSet       = Graph.edgeSet
    path          = Graph.path
    circuit       = Graph.circuit
    clique        = Graph.clique
    biclique      = Graph.biclique
    star          = Graph.star
    tree          = Graph.tree
    forest        = Graph.forest
    mesh          = Graph.mesh
    torus         = Graph.torus
    deBruijn      = Graph.deBruijn
    removeVertex  = Graph.removeVertex
    removeEdge    = Graph.removeEdge
    replaceVertex = Graph.replaceVertex
    mergeVertices = Graph.mergeVertices
    splitVertex   = Graph.splitVertex
    transpose     = Graph.transpose
    induce        = Graph.induce
    simplify      = Graph.simplify
    box           = Graph.box

instance GraphAPI IntAdjacencyMap.IntAdjacencyMap where
    edge          = IntAdjacencyMap.edge
    vertices      = IntAdjacencyMap.vertices
    edges         = IntAdjacencyMap.edges
    overlays      = IntAdjacencyMap.overlays
    connects      = IntAdjacencyMap.connects
    graph         = IntAdjacencyMap.graph
    isSubgraphOf  = IntAdjacencyMap.isSubgraphOf
    isEmpty       = IntAdjacencyMap.isEmpty
    hasVertex     = IntAdjacencyMap.hasVertex
    hasEdge       = IntAdjacencyMap.hasEdge
    vertexCount   = IntAdjacencyMap.vertexCount
    edgeCount     = IntAdjacencyMap.edgeCount
    vertexList    = IntAdjacencyMap.vertexList
    edgeList      = IntAdjacencyMap.edgeList
    vertexIntSet  = IntAdjacencyMap.vertexIntSet
    edgeSet       = IntAdjacencyMap.edgeSet
    path          = IntAdjacencyMap.path
    circuit       = IntAdjacencyMap.circuit
    clique        = IntAdjacencyMap.clique
    biclique      = IntAdjacencyMap.biclique
    star          = IntAdjacencyMap.star
    tree          = IntAdjacencyMap.tree
    forest        = IntAdjacencyMap.forest
    removeVertex  = IntAdjacencyMap.removeVertex
    removeEdge    = IntAdjacencyMap.removeEdge
    replaceVertex = IntAdjacencyMap.replaceVertex
    mergeVertices = IntAdjacencyMap.mergeVertices
    transpose     = IntAdjacencyMap.transpose
    induce        = IntAdjacencyMap.induce

instance Ord a => GraphAPI (Relation.Relation a) where
    edge          = Relation.edge
    vertices      = Relation.vertices
    edges         = Relation.edges
    overlays      = Relation.overlays
    connects      = Relation.connects
    graph         = Relation.graph
    isSubgraphOf  = Relation.isSubgraphOf
    isEmpty       = Relation.isEmpty
    hasVertex     = Relation.hasVertex
    hasEdge       = Relation.hasEdge
    vertexCount   = Relation.vertexCount
    edgeCount     = Relation.edgeCount
    vertexList    = Relation.vertexList
    edgeList      = Relation.edgeList
    vertexSet     = Relation.vertexSet
    edgeSet       = Relation.edgeSet
    path          = Relation.path
    circuit       = Relation.circuit
    clique        = Relation.clique
    biclique      = Relation.biclique
    star          = Relation.star
    tree          = Relation.tree
    forest        = Relation.forest
    removeVertex  = Relation.removeVertex
    removeEdge    = Relation.removeEdge
    replaceVertex = Relation.replaceVertex
    mergeVertices = Relation.mergeVertices
    transpose     = Relation.transpose
    induce        = Relation.induce