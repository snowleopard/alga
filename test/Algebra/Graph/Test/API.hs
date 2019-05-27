{-# LANGUAGE ConstrainedClassMethods, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.API
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- The complete graph API used for generic testing.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.API (
    -- * Graph API
    API (..), Mono (..)
  ) where

import Data.Coerce
import Data.Monoid (Any)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tree
import Test.QuickCheck

import qualified Algebra.Graph                                as G
import qualified Algebra.Graph.AdjacencyIntMap                as AIM
import qualified Algebra.Graph.AdjacencyIntMap.Internal       as AIM
import qualified Algebra.Graph.AdjacencyIntMap.Algorithm      as AIM
import qualified Algebra.Graph.AdjacencyMap                   as AM
import qualified Algebra.Graph.AdjacencyMap.Internal          as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm         as AM
import qualified Algebra.Graph.Labelled                       as LG
import qualified Algebra.Graph.Labelled.AdjacencyMap          as LAM
import qualified Algebra.Graph.Labelled.AdjacencyMap.Internal as LAM
import qualified Algebra.Graph.Relation                       as R
import qualified Algebra.Graph.Relation.Internal              as R
import qualified Algebra.Graph.Relation.Symmetric             as SR
import qualified Algebra.Graph.Relation.Symmetric.Internal    as SR
import qualified Algebra.Graph.ToGraph                        as T

-- | A wrapper for monomorphic data types. We cannot make 'AIM.AdjacencyIntMap'
-- an instance of 'API' directly, but we can if we wrap it into 'Mono'.
newtype Mono g a = Mono { getMono :: g }
    deriving (Arbitrary, Eq, Num, Ord)

instance Show g => Show (Mono g a) where
    show = show . getMono

-- TODO: Add missing API entries for Acyclic, NonEmpty and Symmetric graphs.
-- | The complete graph API specialised to vertices of type 'Int'. A graph data
-- type, such as 'G.Graph', typically implements only a part of the whole API.
class API g where
    empty                      :: g Int
    empty                      = notImplemented
    vertex                     :: Int -> g Int
    vertex                     = notImplemented
    edge                       :: Int -> Int -> g Int
    edge                       = notImplemented
    overlay                    :: g Int -> g Int -> g Int
    overlay                    = notImplemented
    connect                    :: g Int -> g Int -> g Int
    connect                    = notImplemented
    vertices                   :: [Int] -> g Int
    vertices                   = notImplemented
    edges                      :: [(Int, Int)] -> g Int
    edges                      = notImplemented
    overlays                   :: [g Int] -> g Int
    overlays                   = notImplemented
    connects                   :: [g Int] -> g Int
    connects                   = notImplemented
    toGraph                    :: g Int -> G.Graph Int
    toGraph                    = notImplemented
    foldg                      :: b -> (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> g Int -> b
    foldg                      = notImplemented
    isSubgraphOf               :: g Int -> g Int -> Bool
    isSubgraphOf               = notImplemented
    (===)                      :: g Int -> g Int -> Bool
    (===)                      = notImplemented
    isEmpty                    :: g Int -> Bool
    isEmpty                    = notImplemented
    size                       :: g Int -> Int
    size                       = notImplemented
    hasVertex                  :: Int -> g Int -> Bool
    hasVertex                  = notImplemented
    hasEdge                    :: Int -> Int -> g Int -> Bool
    hasEdge                    = notImplemented
    vertexCount                :: g Int -> Int
    vertexCount                = notImplemented
    edgeCount                  :: g Int -> Int
    edgeCount                  = notImplemented
    vertexList                 :: g Int -> [Int]
    vertexList                 = notImplemented
    edgeList                   :: g Int -> [(Int, Int)]
    edgeList                   = notImplemented
    vertexSet                  :: g Int -> Set Int
    vertexSet                  = notImplemented
    vertexIntSet               :: g Int -> IntSet
    vertexIntSet               = notImplemented
    edgeSet                    :: g Int -> Set (Int, Int)
    edgeSet                    = notImplemented
    preSet                     :: Int -> g Int -> Set Int
    preSet                     = notImplemented
    preIntSet                  :: Int -> g Int -> IntSet
    preIntSet                  = notImplemented
    postSet                    :: Int -> g Int -> Set Int
    postSet                    = notImplemented
    postIntSet                 :: Int -> g Int -> IntSet
    postIntSet                 = notImplemented
    neighbours                 :: Int -> g Int -> Set Int
    neighbours                 = notImplemented
    adjacencyList              :: g Int -> [(Int, [Int])]
    adjacencyList              = notImplemented
    adjacencyMap               :: g Int -> Map Int (Set Int)
    adjacencyMap               = notImplemented
    adjacencyIntMap            :: g Int -> IntMap IntSet
    adjacencyIntMap            = notImplemented
    adjacencyMapTranspose      :: g Int -> Map Int (Set Int)
    adjacencyMapTranspose      = notImplemented
    adjacencyIntMapTranspose   :: g Int -> IntMap IntSet
    adjacencyIntMapTranspose   = notImplemented
    dfsForest                  :: g Int -> Forest Int
    dfsForest                  = notImplemented
    dfsForestFrom              :: [Int] -> g Int -> Forest Int
    dfsForestFrom              = notImplemented
    dfs                        :: [Int] -> g Int -> [Int]
    dfs                        = notImplemented
    reachable                  :: Int -> g Int -> [Int]
    reachable                  = notImplemented
    topSort                    :: g Int -> Maybe [Int]
    topSort                    = notImplemented
    isAcyclic                  :: g Int -> Bool
    isAcyclic                  = notImplemented
    toAdjacencyMap             :: g Int -> AM.AdjacencyMap Int
    toAdjacencyMap             = notImplemented
    toAdjacencyIntMap          :: g Int -> AIM.AdjacencyIntMap
    toAdjacencyIntMap          = notImplemented
    toAdjacencyMapTranspose    :: g Int -> AM.AdjacencyMap Int
    toAdjacencyMapTranspose    = notImplemented
    toAdjacencyIntMapTranspose :: g Int -> AIM.AdjacencyIntMap
    toAdjacencyIntMapTranspose = notImplemented
    isDfsForestOf              :: Forest Int -> g Int -> Bool
    isDfsForestOf              = notImplemented
    isTopSortOf                :: [Int] -> g Int -> Bool
    isTopSortOf                = notImplemented
    path                       :: [Int] -> g Int
    path                       = notImplemented
    circuit                    :: [Int] -> g Int
    circuit                    = notImplemented
    clique                     :: [Int] -> g Int
    clique                     = notImplemented
    biclique                   :: [Int] -> [Int] -> g Int
    biclique                   = notImplemented
    star                       :: Int -> [Int] -> g Int
    star                       = notImplemented
    stars                      :: [(Int, [Int])] -> g Int
    stars                      = notImplemented
    tree                       :: Tree (Int) -> g Int
    tree                       = notImplemented
    forest                     :: Forest (Int) -> g Int
    forest                     = notImplemented
    mesh                       :: [Int] -> [Int] -> g (Int, Int)
    mesh                       = notImplemented
    torus                      :: [Int] -> [Int] -> g (Int, Int)
    torus                      = notImplemented
    deBruijn                   :: Int -> [Int] -> g [Int]
    deBruijn                   = notImplemented
    removeVertex               :: Int -> g Int -> g Int
    removeVertex               = notImplemented
    removeEdge                 :: Int -> Int -> g Int -> g Int
    removeEdge                 = notImplemented
    replaceVertex              :: Int -> Int -> g Int -> g Int
    replaceVertex              = notImplemented
    mergeVertices              :: (Int -> Bool) -> Int -> g Int -> g Int
    mergeVertices              = notImplemented
    splitVertex                :: Int -> [Int] -> g Int -> g Int
    splitVertex                = notImplemented
    transpose                  :: g Int -> g Int
    transpose                  = notImplemented
    gmap                       :: (Int -> Int) -> g Int -> g Int
    gmap                       = notImplemented
    gmapPoly                   :: (Ord a, Ord b) => (a -> b) -> g a -> g b
    gmapPoly                   = notImplemented
    bind                       :: g Int -> (Int -> g Int) -> g Int
    bind                       = notImplemented
    induce                     :: (Int -> Bool) -> g Int -> g Int
    induce                     = notImplemented
    simplify                   :: g Int -> g Int
    simplify                   = notImplemented
    compose                    :: g Int -> g Int -> g Int
    compose                    = notImplemented
    box                        :: g Int -> g Int -> g (Int, Int)
    box                        = notImplemented
    closure                    :: g Int -> g Int
    closure                    = notImplemented
    reflexiveClosure           :: g Int -> g Int
    reflexiveClosure           = notImplemented
    symmetricClosure           :: g Int -> g Int
    symmetricClosure           = notImplemented
    transitiveClosure          :: g Int -> g Int
    transitiveClosure          = notImplemented
    consistent                 :: g Int -> Bool
    consistent                 = notImplemented
    fromAdjacencySets          :: [(Int, Set (Int))] -> g Int
    fromAdjacencySets          = notImplemented
    fromAdjacencyIntSets       :: [(Int, IntSet)] -> g Int
    fromAdjacencyIntSets       = notImplemented

notImplemented :: a
notImplemented = error "Not implemented"

instance API AM.AdjacencyMap where
    empty                      = AM.empty
    vertex                     = AM.vertex
    edge                       = AM.edge
    overlay                    = AM.overlay
    connect                    = AM.connect
    vertices                   = AM.vertices
    edges                      = AM.edges
    overlays                   = AM.overlays
    connects                   = AM.connects
    toGraph                    = T.toGraph
    foldg                      = T.foldg
    isSubgraphOf               = AM.isSubgraphOf
    isEmpty                    = AM.isEmpty
    size                       = G.size . toGraph
    hasVertex                  = AM.hasVertex
    hasEdge                    = AM.hasEdge
    vertexCount                = AM.vertexCount
    edgeCount                  = AM.edgeCount
    vertexList                 = AM.vertexList
    edgeList                   = AM.edgeList
    vertexSet                  = AM.vertexSet
    vertexIntSet               = T.vertexIntSet
    edgeSet                    = AM.edgeSet
    preSet                     = AM.preSet
    preIntSet                  = T.preIntSet
    postSet                    = AM.postSet
    postIntSet                 = T.postIntSet
    adjacencyList              = AM.adjacencyList
    adjacencyMap               = AM.adjacencyMap
    adjacencyIntMap            = T.adjacencyIntMap
    adjacencyMapTranspose      = T.adjacencyMapTranspose
    adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    dfsForest                  = AM.dfsForest
    dfsForestFrom              = AM.dfsForestFrom
    dfs                        = AM.dfs
    reachable                  = AM.reachable
    topSort                    = AM.topSort
    isAcyclic                  = AM.isAcyclic
    toAdjacencyMap             = T.toAdjacencyMap
    toAdjacencyIntMap          = T.toAdjacencyIntMap
    toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    isDfsForestOf              = AM.isDfsForestOf
    isTopSortOf                = AM.isTopSortOf
    path                       = AM.path
    circuit                    = AM.circuit
    clique                     = AM.clique
    biclique                   = AM.biclique
    star                       = AM.star
    stars                      = AM.stars
    tree                       = AM.tree
    forest                     = AM.forest
    removeVertex               = AM.removeVertex
    removeEdge                 = AM.removeEdge
    replaceVertex              = AM.replaceVertex
    mergeVertices              = AM.mergeVertices
    transpose                  = AM.transpose
    gmap                       = AM.gmap
    gmapPoly                   = AM.gmap
    induce                     = AM.induce
    compose                    = AM.compose
    closure                    = AM.closure
    reflexiveClosure           = AM.reflexiveClosure
    symmetricClosure           = AM.symmetricClosure
    transitiveClosure          = AM.transitiveClosure
    consistent                 = AM.consistent
    fromAdjacencySets          = AM.fromAdjacencySets

instance API G.Graph where
    empty                      = G.empty
    vertex                     = G.vertex
    edge                       = G.edge
    overlay                    = G.overlay
    connect                    = G.connect
    vertices                   = G.vertices
    edges                      = G.edges
    overlays                   = G.overlays
    connects                   = G.connects
    toGraph                    = id
    foldg                      = G.foldg
    isSubgraphOf               = G.isSubgraphOf
    (===)                      = (G.===)
    isEmpty                    = G.isEmpty
    size                       = G.size
    hasVertex                  = G.hasVertex
    hasEdge                    = G.hasEdge
    vertexCount                = G.vertexCount
    edgeCount                  = G.edgeCount
    vertexList                 = G.vertexList
    edgeList                   = G.edgeList
    vertexSet                  = G.vertexSet
    vertexIntSet               = T.vertexIntSet
    edgeSet                    = G.edgeSet
    preSet                     = T.preSet
    preIntSet                  = T.preIntSet
    postSet                    = T.postSet
    postIntSet                 = T.postIntSet
    adjacencyList              = G.adjacencyList
    adjacencyMap               = T.adjacencyMap
    adjacencyIntMap            = T.adjacencyIntMap
    adjacencyMapTranspose      = T.adjacencyMapTranspose
    adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    dfsForest                  = T.dfsForest
    dfsForestFrom              = T.dfsForestFrom
    dfs                        = T.dfs
    reachable                  = T.reachable
    topSort                    = T.topSort
    isAcyclic                  = T.isAcyclic
    toAdjacencyMap             = T.toAdjacencyMap
    toAdjacencyIntMap          = T.toAdjacencyIntMap
    toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    isDfsForestOf              = T.isDfsForestOf
    isTopSortOf                = T.isTopSortOf
    path                       = G.path
    circuit                    = G.circuit
    clique                     = G.clique
    biclique                   = G.biclique
    star                       = G.star
    stars                      = G.stars
    tree                       = G.tree
    forest                     = G.forest
    mesh                       = G.mesh
    torus                      = G.torus
    deBruijn                   = G.deBruijn
    removeVertex               = G.removeVertex
    removeEdge                 = G.removeEdge
    replaceVertex              = G.replaceVertex
    mergeVertices              = G.mergeVertices
    splitVertex                = G.splitVertex
    transpose                  = G.transpose
    gmap                       = fmap
    gmapPoly                   = fmap
    bind                       = (>>=)
    induce                     = G.induce
    simplify                   = G.simplify
    compose                    = G.compose
    box                        = G.box

instance API (Mono AIM.AdjacencyIntMap) where
    empty                      = coerce AIM.empty
    vertex                     = coerce AIM.vertex
    edge                       = coerce AIM.edge
    overlay                    = coerce AIM.overlay
    connect                    = coerce AIM.connect
    vertices                   = coerce AIM.vertices
    edges                      = coerce AIM.edges
    overlays                   = coerce AIM.overlays
    connects                   = coerce AIM.connects
    toGraph                    = T.toGraph . getMono
    foldg e v o c              = T.foldg e v o c . getMono
    isSubgraphOf               = coerce AIM.isSubgraphOf
    isEmpty                    = coerce AIM.isEmpty
    size                       = G.size . toGraph
    hasVertex                  = coerce AIM.hasVertex
    hasEdge                    = coerce AIM.hasEdge
    vertexCount                = coerce AIM.vertexCount
    edgeCount                  = coerce AIM.edgeCount
    vertexList                 = coerce AIM.vertexList
    edgeList                   = coerce AIM.edgeList
    vertexSet                  = T.vertexSet . getMono
    vertexIntSet               = coerce AIM.vertexIntSet
    edgeSet                    = coerce AIM.edgeSet
    preSet x                   = T.preSet x . getMono
    preIntSet                  = coerce AIM.preIntSet
    postSet x                  = T.postSet x . getMono
    postIntSet                 = coerce AIM.postIntSet
    adjacencyList              = coerce AIM.adjacencyList
    adjacencyMap               = T.adjacencyMap . getMono
    adjacencyIntMap            = coerce AIM.adjacencyIntMap
    adjacencyMapTranspose      = T.adjacencyMapTranspose . getMono
    adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose . getMono
    dfsForest                  = coerce AIM.dfsForest
    dfsForestFrom              = coerce AIM.dfsForestFrom
    dfs                        = coerce AIM.dfs
    reachable                  = coerce AIM.reachable
    topSort                    = coerce AIM.topSort
    isAcyclic                  = coerce AIM.isAcyclic
    toAdjacencyMap             = T.toAdjacencyMap . getMono
    toAdjacencyIntMap          = T.toAdjacencyIntMap . getMono
    toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose . getMono
    toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose . getMono
    isDfsForestOf              = coerce AIM.isDfsForestOf
    isTopSortOf                = coerce AIM.isTopSortOf
    path                       = coerce AIM.path
    circuit                    = coerce AIM.circuit
    clique                     = coerce AIM.clique
    biclique                   = coerce AIM.biclique
    star                       = coerce AIM.star
    stars                      = coerce AIM.stars
    tree                       = coerce AIM.tree
    forest                     = coerce AIM.forest
    removeVertex               = coerce AIM.removeVertex
    removeEdge                 = coerce AIM.removeEdge
    replaceVertex              = coerce AIM.replaceVertex
    mergeVertices              = coerce AIM.mergeVertices
    transpose                  = coerce AIM.transpose
    gmap                       = coerce AIM.gmap
    induce                     = coerce AIM.induce
    compose                    = coerce AIM.compose
    closure                    = coerce AIM.closure
    reflexiveClosure           = coerce AIM.reflexiveClosure
    symmetricClosure           = coerce AIM.symmetricClosure
    transitiveClosure          = coerce AIM.transitiveClosure
    consistent                 = coerce AIM.consistent
    fromAdjacencyIntSets       = coerce AIM.fromAdjacencyIntSets

instance API R.Relation where
    empty                      = R.empty
    vertex                     = R.vertex
    edge                       = R.edge
    overlay                    = R.overlay
    connect                    = R.connect
    vertices                   = R.vertices
    edges                      = R.edges
    overlays                   = R.overlays
    connects                   = R.connects
    toGraph                    = T.toGraph
    foldg                      = T.foldg
    isSubgraphOf               = R.isSubgraphOf
    isEmpty                    = R.isEmpty
    size                       = G.size . toGraph
    hasVertex                  = R.hasVertex
    hasEdge                    = R.hasEdge
    vertexCount                = R.vertexCount
    edgeCount                  = R.edgeCount
    vertexList                 = R.vertexList
    edgeList                   = R.edgeList
    vertexSet                  = R.vertexSet
    vertexIntSet               = T.vertexIntSet
    edgeSet                    = R.edgeSet
    preSet                     = R.preSet
    preIntSet                  = T.preIntSet
    postSet                    = R.postSet
    postIntSet                 = T.postIntSet
    adjacencyList              = R.adjacencyList
    adjacencyMap               = T.adjacencyMap
    adjacencyIntMap            = T.adjacencyIntMap
    adjacencyMapTranspose      = T.adjacencyMapTranspose
    adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    dfsForest                  = T.dfsForest
    dfsForestFrom              = T.dfsForestFrom
    dfs                        = T.dfs
    reachable                  = T.reachable
    topSort                    = T.topSort
    isAcyclic                  = T.isAcyclic
    toAdjacencyMap             = T.toAdjacencyMap
    toAdjacencyIntMap          = T.toAdjacencyIntMap
    toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    isDfsForestOf              = T.isDfsForestOf
    isTopSortOf                = T.isTopSortOf
    path                       = R.path
    circuit                    = R.circuit
    clique                     = R.clique
    biclique                   = R.biclique
    star                       = R.star
    stars                      = R.stars
    tree                       = R.tree
    forest                     = R.forest
    removeVertex               = R.removeVertex
    removeEdge                 = R.removeEdge
    replaceVertex              = R.replaceVertex
    mergeVertices              = R.mergeVertices
    transpose                  = R.transpose
    gmap                       = R.gmap
    gmapPoly                   = R.gmap
    induce                     = R.induce
    compose                    = R.compose
    closure                    = R.closure
    reflexiveClosure           = R.reflexiveClosure
    symmetricClosure           = R.symmetricClosure
    transitiveClosure          = R.transitiveClosure
    consistent                 = R.consistent

instance API SR.Relation where
    empty                      = SR.empty
    vertex                     = SR.vertex
    edge                       = SR.edge
    overlay                    = SR.overlay
    connect                    = SR.connect
    vertices                   = SR.vertices
    edges                      = SR.edges
    overlays                   = SR.overlays
    connects                   = SR.connects
    toGraph                    = T.toGraph
    foldg                      = T.foldg
    isSubgraphOf               = SR.isSubgraphOf
    isEmpty                    = SR.isEmpty
    size                       = G.size . toGraph
    hasVertex                  = SR.hasVertex
    hasEdge                    = SR.hasEdge
    vertexCount                = SR.vertexCount
    edgeCount                  = SR.edgeCount
    vertexList                 = SR.vertexList
    edgeList                   = SR.edgeList
    vertexSet                  = SR.vertexSet
    vertexIntSet               = T.vertexIntSet
    edgeSet                    = SR.edgeSet
    preSet                     = T.preSet
    preIntSet                  = T.preIntSet
    postSet                    = T.postSet
    postIntSet                 = T.postIntSet
    neighbours                 = SR.neighbours
    adjacencyList              = SR.adjacencyList
    adjacencyMap               = T.adjacencyMap
    adjacencyIntMap            = T.adjacencyIntMap
    adjacencyMapTranspose      = T.adjacencyMapTranspose
    adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    dfsForest                  = T.dfsForest
    dfsForestFrom              = T.dfsForestFrom
    dfs                        = T.dfs
    reachable                  = T.reachable
    topSort                    = T.topSort
    isAcyclic                  = T.isAcyclic
    toAdjacencyMap             = T.toAdjacencyMap
    toAdjacencyIntMap          = T.toAdjacencyIntMap
    toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    isDfsForestOf              = T.isDfsForestOf
    isTopSortOf                = T.isTopSortOf
    path                       = SR.path
    circuit                    = SR.circuit
    clique                     = SR.clique
    biclique                   = SR.biclique
    star                       = SR.star
    stars                      = SR.stars
    tree                       = SR.tree
    forest                     = SR.forest
    removeVertex               = SR.removeVertex
    removeEdge                 = SR.removeEdge
    replaceVertex              = SR.replaceVertex
    mergeVertices              = SR.mergeVertices
    transpose                  = id
    gmap                       = SR.gmap
    gmapPoly                   = SR.gmap
    induce                     = SR.induce
    consistent                 = SR.consistent

instance API (LG.Graph Any) where
    empty                      = LG.empty
    vertex                     = LG.vertex
    edge                       = LG.edge mempty
    overlay                    = LG.overlay
    connect                    = LG.connect mempty
    vertices                   = LG.vertices
    edges                      = LG.edges . map (\(x, y) -> (mempty, x, y))
    overlays                   = LG.overlays
    toGraph                    = T.toGraph
    foldg                      = T.foldg
    isSubgraphOf               = LG.isSubgraphOf
    isEmpty                    = LG.isEmpty
    size                       = LG.size
    hasVertex                  = LG.hasVertex
    hasEdge                    = LG.hasEdge
    vertexCount                = T.vertexCount
    edgeCount                  = T.edgeCount
    vertexList                 = LG.vertexList
    edgeList                   = T.edgeList
    vertexSet                  = LG.vertexSet
    vertexIntSet               = T.vertexIntSet
    edgeSet                    = T.edgeSet
    preSet                     = T.preSet
    preIntSet                  = T.preIntSet
    postSet                    = T.postSet
    postIntSet                 = T.postIntSet
    adjacencyList              = T.adjacencyList
    adjacencyMap               = T.adjacencyMap
    adjacencyIntMap            = T.adjacencyIntMap
    adjacencyMapTranspose      = T.adjacencyMapTranspose
    adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    dfsForest                  = T.dfsForest
    dfsForestFrom              = T.dfsForestFrom
    dfs                        = T.dfs
    reachable                  = T.reachable
    topSort                    = T.topSort
    isAcyclic                  = T.isAcyclic
    toAdjacencyMap             = T.toAdjacencyMap
    toAdjacencyIntMap          = T.toAdjacencyIntMap
    toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    isDfsForestOf              = T.isDfsForestOf
    isTopSortOf                = T.isTopSortOf
    removeVertex               = LG.removeVertex
    removeEdge                 = LG.removeEdge
    replaceVertex              = LG.replaceVertex
    transpose                  = LG.transpose
    gmap                       = fmap
    gmapPoly                   = fmap
    induce                     = LG.induce
    closure                    = LG.closure
    reflexiveClosure           = LG.reflexiveClosure
    symmetricClosure           = LG.symmetricClosure
    transitiveClosure          = LG.transitiveClosure

instance API (LAM.AdjacencyMap Any) where
    empty                      = LAM.empty
    vertex                     = LAM.vertex
    edge                       = LAM.edge mempty
    overlay                    = LAM.overlay
    connect                    = LAM.connect mempty
    vertices                   = LAM.vertices
    edges                      = LAM.edges . map (\(x, y) -> (mempty, x, y))
    overlays                   = LAM.overlays
    toGraph                    = T.toGraph
    foldg                      = T.foldg
    isSubgraphOf               = LAM.isSubgraphOf
    isEmpty                    = LAM.isEmpty
    size                       = G.size . toGraph
    hasVertex                  = LAM.hasVertex
    hasEdge                    = LAM.hasEdge
    vertexCount                = LAM.vertexCount
    edgeCount                  = LAM.edgeCount
    vertexList                 = LAM.vertexList
    edgeList                   = T.edgeList
    vertexSet                  = LAM.vertexSet
    vertexIntSet               = T.vertexIntSet
    edgeSet                    = T.edgeSet
    preSet                     = LAM.preSet
    preIntSet                  = T.preIntSet
    postSet                    = LAM.postSet
    postIntSet                 = T.postIntSet
    adjacencyList              = T.adjacencyList
    adjacencyMap               = T.adjacencyMap
    adjacencyIntMap            = T.adjacencyIntMap
    adjacencyMapTranspose      = T.adjacencyMapTranspose
    adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    dfsForest                  = T.dfsForest
    dfsForestFrom              = T.dfsForestFrom
    dfs                        = T.dfs
    reachable                  = T.reachable
    topSort                    = T.topSort
    isAcyclic                  = T.isAcyclic
    toAdjacencyMap             = T.toAdjacencyMap
    toAdjacencyIntMap          = T.toAdjacencyIntMap
    toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    isDfsForestOf              = T.isDfsForestOf
    isTopSortOf                = T.isTopSortOf
    removeVertex               = LAM.removeVertex
    removeEdge                 = LAM.removeEdge
    replaceVertex              = LAM.replaceVertex
    transpose                  = LAM.transpose
    gmap                       = LAM.gmap
    gmapPoly                   = LAM.gmap
    induce                     = LAM.induce
    closure                    = LAM.closure
    reflexiveClosure           = LAM.reflexiveClosure
    symmetricClosure           = LAM.symmetricClosure
    transitiveClosure          = LAM.transitiveClosure
    consistent                 = LAM.consistent
