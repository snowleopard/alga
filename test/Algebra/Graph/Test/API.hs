{-# LANGUAGE ConstraintKinds, CPP, GADTs, RankNTypes, RecordWildCards #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-fields #-}
#endif
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
    API (..), Mono (..), toIntAPI,

    -- * APIs of various graph data types
    adjacencyMapAPI, adjacencyIntMapAPI, graphAPI, relationAPI,
    symmetricRelationAPI, labelledGraphAPI, labelledAdjacencyMapAPI
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
import qualified Algebra.Graph.AdjacencyIntMap.Algorithm      as AIM
import qualified Algebra.Graph.AdjacencyMap                   as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm         as AM
import qualified Algebra.Graph.Labelled                       as LG
import qualified Algebra.Graph.Labelled.AdjacencyMap          as LAM
import qualified Algebra.Graph.Relation                       as R
import qualified Algebra.Graph.Relation.Symmetric             as SR
import qualified Algebra.Graph.ToGraph                        as T

import Algebra.Graph.Test.Arbitrary ()

-- | A wrapper for monomorphic data types. We cannot use 'AIM.AdjacencyIntMap'
-- directly when defining an 'API', but we can if we wrap it into 'Mono'.
newtype Mono g a = Mono { getMono :: g }
    deriving (Arbitrary, Eq, Num, Ord)

instance Show g => Show (Mono g a) where
    show = show . getMono

-- | Convert a polymorphic API dictionary into a monomorphic 'Int' version.
toIntAPI :: API g Ord -> API g ((~) Int)
toIntAPI API{..} = API{..}

-- TODO: Add missing API entries for Acyclic, NonEmpty and Symmetric graphs.
-- | The complete graph API dictionary. A graph data type, such as 'G.Graph',
-- typically implements only a part of the whole API.
data API g c where
    API :: ( Arbitrary (g Int), Num (g Int), Ord (g Int), Ord (g (Int, Int))
           , Ord (g (Int, Char)), Ord (g [Int]), Ord (g [Char])
           , Ord (g (Int, (Int, Int))), Ord (g ((Int, Int), Int))
           , Show (g Int)) =>
        { empty                      :: forall a. c a => g a
        , vertex                     :: forall a. c a => a -> g a
        , edge                       :: forall a. c a => a -> a -> g a
        , overlay                    :: forall a. c a => g a -> g a -> g a
        , connect                    :: forall a. c a => g a -> g a -> g a
        , vertices                   :: forall a. c a => [a] -> g a
        , edges                      :: forall a. c a => [(a, a)] -> g a
        , overlays                   :: forall a. c a => [g a] -> g a
        , connects                   :: forall a. c a => [g a] -> g a
        , toGraph                    :: forall a. c a => g a -> G.Graph a
        , foldg                      :: forall a. c a => forall r. r -> (a -> r) -> (r -> r -> r) -> (r -> r -> r) -> g a -> r
        , isSubgraphOf               :: forall a. c a => g a -> g a -> Bool
        , structEq                   :: forall a. c a => g a -> g a -> Bool
        , isEmpty                    :: forall a. c a => g a -> Bool
        , size                       :: forall a. c a => g a -> Int
        , hasVertex                  :: forall a. c a => a -> g a -> Bool
        , hasEdge                    :: forall a. c a => a -> a -> g a -> Bool
        , vertexCount                :: forall a. c a => g a -> Int
        , edgeCount                  :: forall a. c a => g a -> Int
        , vertexList                 :: forall a. c a => g a -> [a]
        , edgeList                   :: forall a. c a => g a -> [(a, a)]
        , vertexSet                  :: forall a. c a => g a -> Set a
        , vertexIntSet               :: g Int -> IntSet
        , edgeSet                    :: forall a. c a => g a -> Set (a, a)
        , preSet                     :: forall a. c a => a -> g a -> Set a
        , preIntSet                  :: Int -> g Int -> IntSet
        , postSet                    :: forall a. c a => a -> g a -> Set a
        , postIntSet                 :: Int -> g Int -> IntSet
        , neighbours                 :: forall a. c a => a -> g a -> Set a
        , adjacencyList              :: forall a. c a => g a -> [(a, [a])]
        , adjacencyMap               :: forall a. c a => g a -> Map a (Set a)
        , adjacencyIntMap            :: g Int -> IntMap IntSet
        , adjacencyMapTranspose      :: forall a. c a => g a -> Map a (Set a)
        , adjacencyIntMapTranspose   :: g Int -> IntMap IntSet
        , bfsForest                  :: forall a. c a => g a -> Forest a
        , bfsForestFrom              :: forall a. c a => [a] -> g a -> Forest a
        , bfs                        :: forall a. c a => [a] -> g a -> [[a]]
        , dfsForest                  :: forall a. c a => g a -> Forest a
        , dfsForestFrom              :: forall a. c a => [a] -> g a -> Forest a
        , dfs                        :: forall a. c a => [a] -> g a -> [a]
        , reachable                  :: forall a. c a => a -> g a -> [a]
        , topSort                    :: forall a. c a => g a -> Maybe [a]
        , isAcyclic                  :: forall a. c a => g a -> Bool
        , toAdjacencyMap             :: forall a. c a => g a -> AM.AdjacencyMap a
        , toAdjacencyIntMap          :: g Int -> AIM.AdjacencyIntMap
        , toAdjacencyMapTranspose    :: forall a. c a => g a -> AM.AdjacencyMap a
        , toAdjacencyIntMapTranspose :: g Int -> AIM.AdjacencyIntMap
        , isDfsForestOf              :: forall a. c a => Forest a -> g a -> Bool
        , isTopSortOf                :: forall a. c a => [a] -> g a -> Bool
        , path                       :: forall a. c a => [a] -> g a
        , circuit                    :: forall a. c a => [a] -> g a
        , clique                     :: forall a. c a => [a] -> g a
        , biclique                   :: forall a. c a => [a] -> [a] -> g a
        , star                       :: forall a. c a => a -> [a] -> g a
        , stars                      :: forall a. c a => [(a, [a])] -> g a
        , tree                       :: forall a. c a => Tree a -> g a
        , forest                     :: forall a. c a => Forest a -> g a
        , mesh                       :: forall a b. (c a, c b) => [a] -> [b] -> g (a, b)
        , torus                      :: forall a b. (c a, c b) => [a] -> [b] -> g (a, b)
        , deBruijn                   :: forall a. c a => Int -> [a] -> g [a]
        , removeVertex               :: forall a. c a => a -> g a -> g a
        , removeEdge                 :: forall a. c a => a -> a -> g a -> g a
        , replaceVertex              :: forall a. c a => a -> a -> g a -> g a
        , mergeVertices              :: forall a. c a => (a -> Bool) -> a -> g a -> g a
        , splitVertex                :: forall a. c a => a -> [a] -> g a -> g a
        , transpose                  :: forall a. c a => g a -> g a
        , gmap                       :: forall a b. (c a, c b) => (a -> b) -> g a -> g b
        , bind                       :: forall a b. (c a, c b) => g a -> (a -> g b) -> g b
        , induce                     :: forall a. c a => (a -> Bool) -> g a -> g a
        , induceJust                 :: forall a. c a => g (Maybe a) -> g a
        , simplify                   :: forall a. c a => g a -> g a
        , compose                    :: forall a. c a => g a -> g a -> g a
        , box                        :: forall a b. (c a, c b) => g a -> g b -> g (a, b)
        , closure                    :: forall a. c a => g a -> g a
        , reflexiveClosure           :: forall a. c a => g a -> g a
        , symmetricClosure           :: forall a. c a => g a -> g a
        , transitiveClosure          :: forall a. c a => g a -> g a
        , consistent                 :: forall a. c a => g a -> Bool
        , fromAdjacencySets          :: forall a. c a => [(a, Set a)] -> g a
        , fromAdjacencyIntSets       :: [(Int, IntSet)] -> g Int } -> API g c

-- | The API of 'AM.AdjacencyMap'.
adjacencyMapAPI :: API AM.AdjacencyMap Ord
adjacencyMapAPI = API
    { empty                      = AM.empty
    , vertex                     = AM.vertex
    , edge                       = AM.edge
    , overlay                    = AM.overlay
    , connect                    = AM.connect
    , vertices                   = AM.vertices
    , edges                      = AM.edges
    , overlays                   = AM.overlays
    , connects                   = AM.connects
    , toGraph                    = T.toGraph
    , foldg                      = T.foldg
    , isSubgraphOf               = AM.isSubgraphOf
    , isEmpty                    = AM.isEmpty
    , size                       = G.size . T.toGraph
    , hasVertex                  = AM.hasVertex
    , hasEdge                    = AM.hasEdge
    , vertexCount                = AM.vertexCount
    , edgeCount                  = AM.edgeCount
    , vertexList                 = AM.vertexList
    , edgeList                   = AM.edgeList
    , vertexSet                  = AM.vertexSet
    , vertexIntSet               = T.vertexIntSet
    , edgeSet                    = AM.edgeSet
    , preSet                     = AM.preSet
    , preIntSet                  = T.preIntSet
    , postSet                    = AM.postSet
    , postIntSet                 = T.postIntSet
    , adjacencyList              = AM.adjacencyList
    , adjacencyMap               = AM.adjacencyMap
    , adjacencyIntMap            = T.adjacencyIntMap
    , adjacencyMapTranspose      = T.adjacencyMapTranspose
    , adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    , bfsForest                  = AM.bfsForest
    , bfsForestFrom              = AM.bfsForestFrom
    , bfs                        = AM.bfs
    , dfsForest                  = AM.dfsForest
    , dfsForestFrom              = AM.dfsForestFrom
    , dfs                        = AM.dfs
    , reachable                  = AM.reachable
    , topSort                    = AM.topSort
    , isAcyclic                  = AM.isAcyclic
    , toAdjacencyMap             = T.toAdjacencyMap
    , toAdjacencyIntMap          = T.toAdjacencyIntMap
    , toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    , toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    , isDfsForestOf              = AM.isDfsForestOf
    , isTopSortOf                = AM.isTopSortOf
    , path                       = AM.path
    , circuit                    = AM.circuit
    , clique                     = AM.clique
    , biclique                   = AM.biclique
    , star                       = AM.star
    , stars                      = AM.stars
    , tree                       = AM.tree
    , forest                     = AM.forest
    , removeVertex               = AM.removeVertex
    , removeEdge                 = AM.removeEdge
    , replaceVertex              = AM.replaceVertex
    , mergeVertices              = AM.mergeVertices
    , transpose                  = AM.transpose
    , gmap                       = AM.gmap
    , induce                     = AM.induce
    , induceJust                 = AM.induceJust
    , compose                    = AM.compose
    , box                        = AM.box
    , closure                    = AM.closure
    , reflexiveClosure           = AM.reflexiveClosure
    , symmetricClosure           = AM.symmetricClosure
    , transitiveClosure          = AM.transitiveClosure
    , consistent                 = AM.consistent
    , fromAdjacencySets          = AM.fromAdjacencySets }

-- | The API of 'G.Graph'.
graphAPI :: API G.Graph Ord
graphAPI = API
    { empty                      = G.empty
    , vertex                     = G.vertex
    , edge                       = G.edge
    , overlay                    = G.overlay
    , connect                    = G.connect
    , vertices                   = G.vertices
    , edges                      = G.edges
    , overlays                   = G.overlays
    , connects                   = G.connects
    , toGraph                    = id
    , foldg                      = G.foldg
    , isSubgraphOf               = G.isSubgraphOf
    , structEq                   = (G.===)
    , isEmpty                    = G.isEmpty
    , size                       = G.size
    , hasVertex                  = G.hasVertex
    , hasEdge                    = G.hasEdge
    , vertexCount                = G.vertexCount
    , edgeCount                  = G.edgeCount
    , vertexList                 = G.vertexList
    , edgeList                   = G.edgeList
    , vertexSet                  = G.vertexSet
    , vertexIntSet               = T.vertexIntSet
    , edgeSet                    = G.edgeSet
    , preSet                     = T.preSet
    , preIntSet                  = T.preIntSet
    , postSet                    = T.postSet
    , postIntSet                 = T.postIntSet
    , adjacencyList              = G.adjacencyList
    , adjacencyMap               = T.adjacencyMap
    , adjacencyIntMap            = T.adjacencyIntMap
    , adjacencyMapTranspose      = T.adjacencyMapTranspose
    , adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    , dfsForest                  = T.dfsForest
    , dfsForestFrom              = T.dfsForestFrom
    , dfs                        = T.dfs
    , reachable                  = T.reachable
    , topSort                    = T.topSort
    , isAcyclic                  = T.isAcyclic
    , toAdjacencyMap             = T.toAdjacencyMap
    , toAdjacencyIntMap          = T.toAdjacencyIntMap
    , toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    , toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    , isDfsForestOf              = T.isDfsForestOf
    , isTopSortOf                = T.isTopSortOf
    , path                       = G.path
    , circuit                    = G.circuit
    , clique                     = G.clique
    , biclique                   = G.biclique
    , star                       = G.star
    , stars                      = G.stars
    , tree                       = G.tree
    , forest                     = G.forest
    , mesh                       = G.mesh
    , torus                      = G.torus
    , deBruijn                   = G.deBruijn
    , removeVertex               = G.removeVertex
    , removeEdge                 = G.removeEdge
    , replaceVertex              = G.replaceVertex
    , mergeVertices              = G.mergeVertices
    , splitVertex                = G.splitVertex
    , transpose                  = G.transpose
    , gmap                       = fmap
    , bind                       = (>>=)
    , induce                     = G.induce
    , induceJust                 = G.induceJust
    , simplify                   = G.simplify
    , compose                    = G.compose
    , box                        = G.box }

-- | The API of 'AIM.AdjacencyIntMap'.
adjacencyIntMapAPI :: API (Mono AIM.AdjacencyIntMap) ((~) Int)
adjacencyIntMapAPI = API
    { empty                      = coerce AIM.empty
    , vertex                     = coerce AIM.vertex
    , edge                       = coerce AIM.edge
    , overlay                    = coerce AIM.overlay
    , connect                    = coerce AIM.connect
    , vertices                   = coerce AIM.vertices
    , edges                      = coerce AIM.edges
    , overlays                   = coerce AIM.overlays
    , connects                   = coerce AIM.connects
    , toGraph                    = T.toGraph . getMono
    , foldg                      = \e v o c -> T.foldg e v o c . getMono
    , isSubgraphOf               = coerce AIM.isSubgraphOf
    , isEmpty                    = coerce AIM.isEmpty
    , size                       = G.size . T.toGraph . getMono
    , hasVertex                  = coerce AIM.hasVertex
    , hasEdge                    = coerce AIM.hasEdge
    , vertexCount                = coerce AIM.vertexCount
    , edgeCount                  = coerce AIM.edgeCount
    , vertexList                 = coerce AIM.vertexList
    , edgeList                   = coerce AIM.edgeList
    , vertexSet                  = T.vertexSet . getMono
    , vertexIntSet               = coerce AIM.vertexIntSet
    , edgeSet                    = coerce AIM.edgeSet
    , preSet                     = \x -> T.preSet x . getMono
    , preIntSet                  = coerce AIM.preIntSet
    , postSet                    = \x -> T.postSet x . getMono
    , postIntSet                 = coerce AIM.postIntSet
    , adjacencyList              = coerce AIM.adjacencyList
    , adjacencyMap               = T.adjacencyMap . getMono
    , adjacencyIntMap            = coerce AIM.adjacencyIntMap
    , adjacencyMapTranspose      = T.adjacencyMapTranspose . getMono
    , adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose . getMono
    , bfsForest                  = coerce AIM.bfsForest
    , bfsForestFrom              = coerce AIM.bfsForestFrom
    , bfs                        = coerce AIM.bfs
    , dfsForest                  = coerce AIM.dfsForest
    , dfsForestFrom              = coerce AIM.dfsForestFrom
    , dfs                        = coerce AIM.dfs
    , reachable                  = coerce AIM.reachable
    , topSort                    = coerce AIM.topSort
    , isAcyclic                  = coerce AIM.isAcyclic
    , toAdjacencyMap             = T.toAdjacencyMap . getMono
    , toAdjacencyIntMap          = T.toAdjacencyIntMap . getMono
    , toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose . getMono
    , toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose . getMono
    , isDfsForestOf              = coerce AIM.isDfsForestOf
    , isTopSortOf                = coerce AIM.isTopSortOf
    , path                       = coerce AIM.path
    , circuit                    = coerce AIM.circuit
    , clique                     = coerce AIM.clique
    , biclique                   = coerce AIM.biclique
    , star                       = coerce AIM.star
    , stars                      = coerce AIM.stars
    , tree                       = coerce AIM.tree
    , forest                     = coerce AIM.forest
    , removeVertex               = coerce AIM.removeVertex
    , removeEdge                 = coerce AIM.removeEdge
    , replaceVertex              = coerce AIM.replaceVertex
    , mergeVertices              = coerce AIM.mergeVertices
    , transpose                  = coerce AIM.transpose
    , gmap                       = coerce AIM.gmap
    , induce                     = coerce AIM.induce
    , compose                    = coerce AIM.compose
    , closure                    = coerce AIM.closure
    , reflexiveClosure           = coerce AIM.reflexiveClosure
    , symmetricClosure           = coerce AIM.symmetricClosure
    , transitiveClosure          = coerce AIM.transitiveClosure
    , consistent                 = coerce AIM.consistent
    , fromAdjacencyIntSets       = coerce AIM.fromAdjacencyIntSets }

-- | The API of 'R.Relation'.
relationAPI :: API R.Relation Ord
relationAPI = API
    { empty                      = R.empty
    , vertex                     = R.vertex
    , edge                       = R.edge
    , overlay                    = R.overlay
    , connect                    = R.connect
    , vertices                   = R.vertices
    , edges                      = R.edges
    , overlays                   = R.overlays
    , connects                   = R.connects
    , toGraph                    = T.toGraph
    , foldg                      = T.foldg
    , isSubgraphOf               = R.isSubgraphOf
    , isEmpty                    = R.isEmpty
    , size                       = G.size . T.toGraph
    , hasVertex                  = R.hasVertex
    , hasEdge                    = R.hasEdge
    , vertexCount                = R.vertexCount
    , edgeCount                  = R.edgeCount
    , vertexList                 = R.vertexList
    , edgeList                   = R.edgeList
    , vertexSet                  = R.vertexSet
    , vertexIntSet               = T.vertexIntSet
    , edgeSet                    = R.edgeSet
    , preSet                     = R.preSet
    , preIntSet                  = T.preIntSet
    , postSet                    = R.postSet
    , postIntSet                 = T.postIntSet
    , adjacencyList              = R.adjacencyList
    , adjacencyMap               = T.adjacencyMap
    , adjacencyIntMap            = T.adjacencyIntMap
    , adjacencyMapTranspose      = T.adjacencyMapTranspose
    , adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    , dfsForest                  = T.dfsForest
    , dfsForestFrom              = T.dfsForestFrom
    , dfs                        = T.dfs
    , reachable                  = T.reachable
    , topSort                    = T.topSort
    , isAcyclic                  = T.isAcyclic
    , toAdjacencyMap             = T.toAdjacencyMap
    , toAdjacencyIntMap          = T.toAdjacencyIntMap
    , toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    , toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    , isDfsForestOf              = T.isDfsForestOf
    , isTopSortOf                = T.isTopSortOf
    , path                       = R.path
    , circuit                    = R.circuit
    , clique                     = R.clique
    , biclique                   = R.biclique
    , star                       = R.star
    , stars                      = R.stars
    , tree                       = R.tree
    , forest                     = R.forest
    , removeVertex               = R.removeVertex
    , removeEdge                 = R.removeEdge
    , replaceVertex              = R.replaceVertex
    , mergeVertices              = R.mergeVertices
    , transpose                  = R.transpose
    , gmap                       = R.gmap
    , induce                     = R.induce
    , induceJust                 = R.induceJust
    , compose                    = R.compose
    , closure                    = R.closure
    , reflexiveClosure           = R.reflexiveClosure
    , symmetricClosure           = R.symmetricClosure
    , transitiveClosure          = R.transitiveClosure
    , consistent                 = R.consistent }

-- | The API of 'SR.Relation'.
symmetricRelationAPI :: API SR.Relation Ord
symmetricRelationAPI = API
    { empty                      = SR.empty
    , vertex                     = SR.vertex
    , edge                       = SR.edge
    , overlay                    = SR.overlay
    , connect                    = SR.connect
    , vertices                   = SR.vertices
    , edges                      = SR.edges
    , overlays                   = SR.overlays
    , connects                   = SR.connects
    , toGraph                    = T.toGraph
    , foldg                      = T.foldg
    , isSubgraphOf               = SR.isSubgraphOf
    , isEmpty                    = SR.isEmpty
    , size                       = G.size . T.toGraph
    , hasVertex                  = SR.hasVertex
    , hasEdge                    = SR.hasEdge
    , vertexCount                = SR.vertexCount
    , edgeCount                  = SR.edgeCount
    , vertexList                 = SR.vertexList
    , edgeList                   = SR.edgeList
    , vertexSet                  = SR.vertexSet
    , vertexIntSet               = T.vertexIntSet
    , edgeSet                    = SR.edgeSet
    , preSet                     = T.preSet
    , preIntSet                  = T.preIntSet
    , postSet                    = T.postSet
    , postIntSet                 = T.postIntSet
    , neighbours                 = SR.neighbours
    , adjacencyList              = SR.adjacencyList
    , adjacencyMap               = T.adjacencyMap
    , adjacencyIntMap            = T.adjacencyIntMap
    , adjacencyMapTranspose      = T.adjacencyMapTranspose
    , adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    , dfsForest                  = T.dfsForest
    , dfsForestFrom              = T.dfsForestFrom
    , dfs                        = T.dfs
    , reachable                  = T.reachable
    , topSort                    = T.topSort
    , isAcyclic                  = T.isAcyclic
    , toAdjacencyMap             = T.toAdjacencyMap
    , toAdjacencyIntMap          = T.toAdjacencyIntMap
    , toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    , toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    , isDfsForestOf              = T.isDfsForestOf
    , isTopSortOf                = T.isTopSortOf
    , path                       = SR.path
    , circuit                    = SR.circuit
    , clique                     = SR.clique
    , biclique                   = SR.biclique
    , star                       = SR.star
    , stars                      = SR.stars
    , tree                       = SR.tree
    , forest                     = SR.forest
    , removeVertex               = SR.removeVertex
    , removeEdge                 = SR.removeEdge
    , replaceVertex              = SR.replaceVertex
    , mergeVertices              = SR.mergeVertices
    , transpose                  = id
    , gmap                       = SR.gmap
    , induce                     = SR.induce
    , induceJust                 = SR.induceJust
    , consistent                 = SR.consistent }

-- | The API of 'LG.Graph'.
labelledGraphAPI :: API (LG.Graph Any) Ord
labelledGraphAPI = API
    { empty                      = LG.empty
    , vertex                     = LG.vertex
    , edge                       = LG.edge mempty
    , overlay                    = LG.overlay
    , connect                    = LG.connect mempty
    , vertices                   = LG.vertices
    , edges                      = LG.edges . map (\(x, y) -> (mempty, x, y))
    , overlays                   = LG.overlays
    , toGraph                    = T.toGraph
    , foldg                      = T.foldg
    , isSubgraphOf               = LG.isSubgraphOf
    , isEmpty                    = LG.isEmpty
    , size                       = LG.size
    , hasVertex                  = LG.hasVertex
    , hasEdge                    = LG.hasEdge
    , vertexCount                = T.vertexCount
    , edgeCount                  = T.edgeCount
    , vertexList                 = LG.vertexList
    , edgeList                   = T.edgeList
    , vertexSet                  = LG.vertexSet
    , vertexIntSet               = T.vertexIntSet
    , edgeSet                    = T.edgeSet
    , preSet                     = T.preSet
    , preIntSet                  = T.preIntSet
    , postSet                    = T.postSet
    , postIntSet                 = T.postIntSet
    , adjacencyList              = T.adjacencyList
    , adjacencyMap               = T.adjacencyMap
    , adjacencyIntMap            = T.adjacencyIntMap
    , adjacencyMapTranspose      = T.adjacencyMapTranspose
    , adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    , dfsForest                  = T.dfsForest
    , dfsForestFrom              = T.dfsForestFrom
    , dfs                        = T.dfs
    , reachable                  = T.reachable
    , topSort                    = T.topSort
    , isAcyclic                  = T.isAcyclic
    , toAdjacencyMap             = T.toAdjacencyMap
    , toAdjacencyIntMap          = T.toAdjacencyIntMap
    , toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    , toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    , isDfsForestOf              = T.isDfsForestOf
    , isTopSortOf                = T.isTopSortOf
    , removeVertex               = LG.removeVertex
    , removeEdge                 = LG.removeEdge
    , replaceVertex              = LG.replaceVertex
    , transpose                  = LG.transpose
    , gmap                       = fmap
    , induce                     = LG.induce
    , induceJust                 = LG.induceJust
    , closure                    = LG.closure
    , reflexiveClosure           = LG.reflexiveClosure
    , symmetricClosure           = LG.symmetricClosure
    , transitiveClosure          = LG.transitiveClosure }

-- | The API of 'LAM.AdjacencyMap'.
labelledAdjacencyMapAPI :: API (LAM.AdjacencyMap Any) Ord
labelledAdjacencyMapAPI = API
    { empty                      = LAM.empty
    , vertex                     = LAM.vertex
    , edge                       = LAM.edge mempty
    , overlay                    = LAM.overlay
    , connect                    = LAM.connect mempty
    , vertices                   = LAM.vertices
    , edges                      = LAM.edges . map (\(x, y) -> (mempty, x, y))
    , overlays                   = LAM.overlays
    , toGraph                    = T.toGraph
    , foldg                      = T.foldg
    , isSubgraphOf               = LAM.isSubgraphOf
    , isEmpty                    = LAM.isEmpty
    , size                       = G.size . T.toGraph
    , hasVertex                  = LAM.hasVertex
    , hasEdge                    = LAM.hasEdge
    , vertexCount                = LAM.vertexCount
    , edgeCount                  = LAM.edgeCount
    , vertexList                 = LAM.vertexList
    , edgeList                   = T.edgeList
    , vertexSet                  = LAM.vertexSet
    , vertexIntSet               = T.vertexIntSet
    , edgeSet                    = T.edgeSet
    , preSet                     = LAM.preSet
    , preIntSet                  = T.preIntSet
    , postSet                    = LAM.postSet
    , postIntSet                 = T.postIntSet
    , adjacencyList              = T.adjacencyList
    , adjacencyMap               = T.adjacencyMap
    , adjacencyIntMap            = T.adjacencyIntMap
    , adjacencyMapTranspose      = T.adjacencyMapTranspose
    , adjacencyIntMapTranspose   = T.adjacencyIntMapTranspose
    , dfsForest                  = T.dfsForest
    , dfsForestFrom              = T.dfsForestFrom
    , dfs                        = T.dfs
    , reachable                  = T.reachable
    , topSort                    = T.topSort
    , isAcyclic                  = T.isAcyclic
    , toAdjacencyMap             = T.toAdjacencyMap
    , toAdjacencyIntMap          = T.toAdjacencyIntMap
    , toAdjacencyMapTranspose    = T.toAdjacencyMapTranspose
    , toAdjacencyIntMapTranspose = T.toAdjacencyIntMapTranspose
    , isDfsForestOf              = T.isDfsForestOf
    , isTopSortOf                = T.isTopSortOf
    , removeVertex               = LAM.removeVertex
    , removeEdge                 = LAM.removeEdge
    , replaceVertex              = LAM.replaceVertex
    , transpose                  = LAM.transpose
    , gmap                       = LAM.gmap
    , induce                     = LAM.induce
    , induceJust                 = LAM.induceJust
    , closure                    = LAM.closure
    , reflexiveClosure           = LAM.reflexiveClosure
    , symmetricClosure           = LAM.symmetricClosure
    , transitiveClosure          = LAM.transitiveClosure
    , consistent                 = LAM.consistent }
