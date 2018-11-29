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

import Data.Monoid (Any)
import Data.Tree

import Algebra.Graph.Class (Graph (..))

import qualified Algebra.Graph                       as Graph
import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import qualified Algebra.Graph.Fold                  as Fold
import qualified Algebra.Graph.HigherKinded.Class    as HClass
import qualified Algebra.Graph.AdjacencyIntMap       as AIM
import qualified Algebra.Graph.Relation              as R
import qualified Data.Set                            as Set
import qualified Data.IntSet                         as IntSet

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
    compose              :: g -> g -> g
    compose              = notImplemented
    closure              :: g -> g
    closure              = notImplemented
    reflexiveClosure     :: g -> g
    reflexiveClosure     = notImplemented
    symmetricClosure     :: g -> g
    symmetricClosure     = notImplemented
    transitiveClosure    :: g -> g
    transitiveClosure    = notImplemented
    bind                 :: Vertex g ~ Int => g -> (Int -> g) -> g
    bind                 = notImplemented
    simplify             :: g -> g
    simplify             = notImplemented
    box                  :: forall a b f. (Vertex (f a) ~ a, Vertex (f b) ~ b, Vertex (f (a, b)) ~ (a, b), g ~ f (a, b)) => f a -> f b -> f (a, b)
    box                  = notImplemented

notImplemented :: a
notImplemented = error "Not implemented"

instance Ord a => GraphAPI (AM.AdjacencyMap a) where
    edge              = AM.edge
    vertices          = AM.vertices
    edges             = AM.edges
    overlays          = AM.overlays
    connects          = AM.connects
    fromAdjacencySets = AM.fromAdjacencySets
    isSubgraphOf      = AM.isSubgraphOf
    path              = AM.path
    circuit           = AM.circuit
    clique            = AM.clique
    biclique          = AM.biclique
    star              = AM.star
    stars             = AM.stars
    tree              = AM.tree
    forest            = AM.forest
    removeVertex      = AM.removeVertex
    removeEdge        = AM.removeEdge
    replaceVertex     = AM.replaceVertex
    mergeVertices     = AM.mergeVertices
    transpose         = AM.transpose
    gmap              = AM.gmap
    induce            = AM.induce
    compose           = AM.compose
    closure           = AM.closure
    reflexiveClosure  = AM.reflexiveClosure
    symmetricClosure  = AM.symmetricClosure
    transitiveClosure = AM.transitiveClosure

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
    compose       = Graph.compose
    bind          = (>>=)
    simplify      = Graph.simplify
    box           = Graph.box

instance GraphAPI AIM.AdjacencyIntMap where
    edge                 = AIM.edge
    vertices             = AIM.vertices
    edges                = AIM.edges
    overlays             = AIM.overlays
    connects             = AIM.connects
    fromAdjacencyIntSets = AIM.fromAdjacencyIntSets
    isSubgraphOf         = AIM.isSubgraphOf
    path                 = AIM.path
    circuit              = AIM.circuit
    clique               = AIM.clique
    biclique             = AIM.biclique
    star                 = AIM.star
    stars                = AIM.stars
    tree                 = AIM.tree
    forest               = AIM.forest
    removeVertex         = AIM.removeVertex
    removeEdge           = AIM.removeEdge
    replaceVertex        = AIM.replaceVertex
    mergeVertices        = AIM.mergeVertices
    transpose            = AIM.transpose
    gmap                 = AIM.gmap
    induce               = AIM.induce
    compose              = AIM.compose
    closure              = AIM.closure
    reflexiveClosure     = AIM.reflexiveClosure
    symmetricClosure     = AIM.symmetricClosure
    transitiveClosure    = AIM.transitiveClosure

instance Ord a => GraphAPI (R.Relation a) where
    edge              = R.edge
    vertices          = R.vertices
    edges             = R.edges
    overlays          = R.overlays
    connects          = R.connects
    isSubgraphOf      = R.isSubgraphOf
    path              = R.path
    circuit           = R.circuit
    clique            = R.clique
    biclique          = R.biclique
    star              = R.star
    stars             = R.stars
    tree              = R.tree
    forest            = R.forest
    removeVertex      = R.removeVertex
    removeEdge        = R.removeEdge
    replaceVertex     = R.replaceVertex
    mergeVertices     = R.mergeVertices
    transpose         = R.transpose
    gmap              = R.gmap
    induce            = R.induce
    compose           = R.compose
    closure           = R.closure
    reflexiveClosure  = R.reflexiveClosure
    symmetricClosure  = R.symmetricClosure
    transitiveClosure = R.transitiveClosure

instance Ord a => GraphAPI (LAM.AdjacencyMap Any a) where
    vertices     = LAM.vertices
    overlays     = LAM.overlays
    isSubgraphOf = LAM.isSubgraphOf
    removeVertex = LAM.removeVertex
    induce       = LAM.induce
