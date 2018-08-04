{-# LANGUAGE ConstrainedClassMethods, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.API
-- Copyright  : (c) Andrey Mokhov 2016-2018
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

import Data.Tree

import Algebra.Graph.Class (Graph (..))

import qualified Algebra.Graph                          as Graph
import qualified Algebra.Graph.AdjacencyMap             as AdjacencyMap
import qualified Algebra.Graph.AdjacencyMap.Internal    as AdjacencyMap
import qualified Algebra.Graph.Fold                     as Fold
import qualified Algebra.Graph.HigherKinded.Class       as HClass
import qualified Algebra.Graph.AdjacencyIntMap          as AdjacencyIntMap
import qualified Algebra.Graph.AdjacencyIntMap.Internal as AdjacencyIntMap
import qualified Algebra.Graph.Relation                 as Relation
import qualified Data.Set                               as Set
import qualified Data.IntSet                            as IntSet

class Graph g => GraphAPI g where
    edge                 :: Vertex g -> Vertex g -> g
    edge                 = notImplemented
    vertices             :: [Vertex g] -> g
    vertices             = notImplemented
    edges                :: [(Vertex g, Vertex g)] -> g
    edges                = notImplemented
    overlays             :: [g] -> g
    overlays             = notImplemented
    connects             :: [g] -> g
    connects             = notImplemented
    fromAdjacencySets    :: [(Vertex g, Set.Set (Vertex g))] -> g
    fromAdjacencySets    = notImplemented
    fromAdjacencyIntSets :: [(Int, IntSet.IntSet)] -> g
    fromAdjacencyIntSets = notImplemented
    isSubgraphOf         :: g -> g -> Bool
    isSubgraphOf         = notImplemented
    (===)                :: g -> g -> Bool
    (===)                = notImplemented
    path                 :: [Vertex g] -> g
    path                 = notImplemented
    circuit              :: [Vertex g] -> g
    circuit              = notImplemented
    clique               :: [Vertex g] -> g
    clique               = notImplemented
    biclique             :: [Vertex g] -> [Vertex g] -> g
    biclique             = notImplemented
    star                 :: Vertex g -> [Vertex g] -> g
    star                 = notImplemented
    stars                :: [(Vertex g, [Vertex g])] -> g
    stars                = notImplemented
    starTranspose        :: Vertex g -> [Vertex g] -> g
    starTranspose        = notImplemented
    tree                 :: Tree (Vertex g) -> g
    tree                 = notImplemented
    forest               :: Forest (Vertex g) -> g
    forest               = notImplemented
    mesh                 :: Vertex g ~ (a, b) => [a] -> [b] -> g
    mesh                 = notImplemented
    torus                :: Vertex g ~ (a, b) => [a] -> [b] -> g
    torus                = notImplemented
    deBruijn             :: Vertex g ~ [a] => Int -> [a] -> g
    deBruijn             = notImplemented
    removeVertex         :: Vertex g -> g -> g
    removeVertex         = notImplemented
    removeEdge           :: Vertex g -> Vertex g -> g -> g
    removeEdge           = notImplemented
    replaceVertex        :: Vertex g -> Vertex g -> g -> g
    replaceVertex        = notImplemented
    mergeVertices        :: (Vertex g -> Bool) -> Vertex g -> g -> g
    mergeVertices        = notImplemented
    splitVertex          :: Vertex g -> [Vertex g] -> g -> g
    splitVertex          = notImplemented
    transpose            :: g -> g
    transpose            = notImplemented
    gmap                 :: Vertex g ~ Int => (Int -> Int) -> g -> g
    gmap                 = notImplemented
    induce               :: (Vertex g -> Bool) -> g -> g
    induce               = notImplemented
    bind                 :: Vertex g ~ Int => g -> (Int -> g) -> g
    bind                 = notImplemented
    simplify             :: g -> g
    simplify             = notImplemented
    box                  :: forall a b f. (Vertex (f a) ~ a, Vertex (f b) ~ b, Vertex (f (a, b)) ~ (a, b), g ~ f (a, b)) => f a -> f b -> f (a, b)
    box                  = notImplemented

notImplemented :: a
notImplemented = error "Not implemented"

instance Ord a => GraphAPI (AdjacencyMap.AdjacencyMap a) where
    edge              = AdjacencyMap.edge
    vertices          = AdjacencyMap.vertices
    edges             = AdjacencyMap.edges
    overlays          = AdjacencyMap.overlays
    connects          = AdjacencyMap.connects
    fromAdjacencySets = AdjacencyMap.fromAdjacencySets
    isSubgraphOf      = AdjacencyMap.isSubgraphOf
    path              = AdjacencyMap.path
    circuit           = AdjacencyMap.circuit
    clique            = AdjacencyMap.clique
    biclique          = AdjacencyMap.biclique
    star              = AdjacencyMap.star
    stars             = AdjacencyMap.stars
    starTranspose     = AdjacencyMap.starTranspose
    tree              = AdjacencyMap.tree
    forest            = AdjacencyMap.forest
    removeVertex      = AdjacencyMap.removeVertex
    removeEdge        = AdjacencyMap.removeEdge
    replaceVertex     = AdjacencyMap.replaceVertex
    mergeVertices     = AdjacencyMap.mergeVertices
    transpose         = AdjacencyMap.transpose
    gmap              = AdjacencyMap.gmap
    induce            = AdjacencyMap.induce

instance Ord a => GraphAPI (Fold.Fold a) where
    edge          = Fold.edge
    vertices      = Fold.vertices
    edges         = Fold.edges
    overlays      = Fold.overlays
    connects      = Fold.connects
    isSubgraphOf  = Fold.isSubgraphOf
    path          = Fold.path
    circuit       = Fold.circuit
    clique        = Fold.clique
    biclique      = Fold.biclique
    star          = Fold.star
    stars         = Fold.stars
    starTranspose = Fold.starTranspose
    tree          = HClass.tree
    forest        = HClass.forest
    mesh          = HClass.mesh
    torus         = HClass.torus
    deBruijn      = HClass.deBruijn
    removeVertex  = Fold.removeVertex
    removeEdge    = Fold.removeEdge
    replaceVertex = HClass.replaceVertex
    mergeVertices = HClass.mergeVertices
    splitVertex   = HClass.splitVertex
    transpose     = Fold.transpose
    gmap          = fmap
    induce        = Fold.induce
    bind          = (>>=)
    simplify      = Fold.simplify
    box           = HClass.box

instance Ord a => GraphAPI (Graph.Graph a) where
    edge          = Graph.edge
    vertices      = Graph.vertices
    edges         = Graph.edges
    overlays      = Graph.overlays
    connects      = Graph.connects
    isSubgraphOf  = Graph.isSubgraphOf
    (===)         = (Graph.===)
    path          = Graph.path
    circuit       = Graph.circuit
    clique        = Graph.clique
    biclique      = Graph.biclique
    star          = Graph.star
    stars         = Graph.stars
    starTranspose = Graph.starTranspose
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
    gmap          = fmap
    induce        = Graph.induce
    bind          = (>>=)
    simplify      = Graph.simplify
    box           = Graph.box

instance GraphAPI AdjacencyIntMap.AdjacencyIntMap where
    edge                 = AdjacencyIntMap.edge
    vertices             = AdjacencyIntMap.vertices
    edges                = AdjacencyIntMap.edges
    overlays             = AdjacencyIntMap.overlays
    connects             = AdjacencyIntMap.connects
    fromAdjacencyIntSets = AdjacencyIntMap.fromAdjacencyIntSets
    isSubgraphOf         = AdjacencyIntMap.isSubgraphOf
    path                 = AdjacencyIntMap.path
    circuit              = AdjacencyIntMap.circuit
    clique               = AdjacencyIntMap.clique
    biclique             = AdjacencyIntMap.biclique
    star                 = AdjacencyIntMap.star
    stars                = AdjacencyIntMap.stars
    starTranspose        = AdjacencyIntMap.starTranspose
    tree                 = AdjacencyIntMap.tree
    forest               = AdjacencyIntMap.forest
    removeVertex         = AdjacencyIntMap.removeVertex
    removeEdge           = AdjacencyIntMap.removeEdge
    replaceVertex        = AdjacencyIntMap.replaceVertex
    mergeVertices        = AdjacencyIntMap.mergeVertices
    transpose            = AdjacencyIntMap.transpose
    gmap                 = AdjacencyIntMap.gmap
    induce               = AdjacencyIntMap.induce

instance Ord a => GraphAPI (Relation.Relation a) where
    edge          = Relation.edge
    vertices      = Relation.vertices
    edges         = Relation.edges
    overlays      = Relation.overlays
    connects      = Relation.connects
    isSubgraphOf  = Relation.isSubgraphOf
    path          = Relation.path
    circuit       = Relation.circuit
    clique        = Relation.clique
    biclique      = Relation.biclique
    star          = Relation.star
    stars         = Relation.stars
    starTranspose = Relation.starTranspose
    tree          = Relation.tree
    forest        = Relation.forest
    removeVertex  = Relation.removeVertex
    removeEdge    = Relation.removeEdge
    replaceVertex = Relation.replaceVertex
    mergeVertices = Relation.mergeVertices
    transpose     = Relation.transpose
    gmap          = Relation.gmap
    induce        = Relation.induce
