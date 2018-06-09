{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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

import qualified Algebra.Graph                 as G
import qualified Algebra.Graph.AdjacencyMap    as AM
import qualified Algebra.Graph.IntAdjacencyMap as IAM
import qualified Algebra.Graph.Relation        as R
import qualified Data.IntMap                   as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | The 'ToGraph' type class captures data types that can be converted to
-- polymorphic graph expressions. The conversion method 'toGraph' semantically
-- acts as the identity on graph data structures, but allows to convert graphs
-- between different data representations.
--
-- @
--       toGraph (g     :: 'Algebra.Graph.Graph' a  ) :: 'Algebra.Graph.Graph' a       == g
-- 'show' (toGraph (1 * 2 :: 'Algebra.Graph.Graph' Int) :: 'Algebra.Graph.Relation' Int) == "edge 1 2"
-- @
--
-- The second method 'foldg' is used for generalised graph folding. It recursively
-- collapses a given data type by applying the provided graph construction
-- primitives. The order of arguments is: empty, vertex, overlay and connect,
-- and it is assumed that the functions satisfy the axioms of the algebra.
--
-- @
-- foldg 'empty' 'vertex'        'overlay' 'connect'        == id
-- foldg 'empty' 'vertex'        'overlay' (flip 'connect') == 'transpose'
-- foldg []    pure          (++)    (++)           == 'Data.Foldable.toList'
-- foldg 0     (const 1)     (+)     (+)            == 'Data.Foldable.length'
-- foldg 1     (const 1)     (+)     (+)            == 'size'
-- foldg True  (const False) (&&)    (&&)           == 'isEmpty'
-- @
--
-- The following law establishes the relation between 'toGraph' and 'foldg':
--
-- @
-- toGraph == foldg 'Empty' 'Vertex' 'Overlay' 'Connect'
-- @
class ToGraph t where
    type ToVertex t
    toGraph :: t -> G.Graph (ToVertex t)
    toGraph = foldg G.Empty G.Vertex G.Overlay G.Connect
    foldg :: r -> (ToVertex t -> r) -> (r -> r -> r) -> (r -> r -> r) -> t -> r
    foldg e v o c = go . toGraph
      where
        go G.Empty         = e
        go (G.Vertex x   ) = v x
        go (G.Overlay x y) = o (go x) (go y)
        go (G.Connect x y) = c (go x) (go y)

instance ToGraph (G.Graph a) where
    type ToVertex (G.Graph a) = a
    toGraph = id
    foldg   = G.foldg

-- TODO: Move to Algebra.Graph
fromAdjacencyList :: [(a, [a])] -> G.Graph a
fromAdjacencyList = G.overlays . map (uncurry G.star)

instance ToGraph (AM.AdjacencyMap a) where
    type ToVertex (AM.AdjacencyMap a) = a
    toGraph = fromAdjacencyList . map (fmap Set.toList) . Map.toList . AM.adjacencyMap

instance ToGraph IAM.IntAdjacencyMap where
    type ToVertex IAM.IntAdjacencyMap = Int
    toGraph = G.overlays . map (uncurry G.star . fmap IntSet.toList) . IntMap.toList . IAM.adjacencyMap

-- TODO: Optimise
instance ToGraph (R.Relation a) where
    type ToVertex (R.Relation a) = a
    toGraph r = G.vertices (Set.toList $ R.domain r) `G.overlay` G.edges (Set.toList $ R.relation r)
