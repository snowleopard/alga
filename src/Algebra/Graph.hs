-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- A library for algebraic construction and manipulation of graphs in Haskell.
-- See <https://github.com/snowleopard/alga-paper this paper> for the motivation
-- behind the library, the underlying theory and implementation details.
--
-- See "Algebra.Graph.HigherKinded" for an alternative definition of the core
-- type class 'Graph', where it is higher-kinded.
-----------------------------------------------------------------------------
module Algebra.Graph (
    -- * The core type class
    Graph (..),

    -- * Basic graph construction primitives
    vertices, overlays, connects, edge, edges, graph,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest, deBruijn,

    -- * Graph folding
    Fold, foldg,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, toSet, toIntSet,

    -- * Graph transformation
    transpose, simplify, gmap, replaceVertex, mergeVertices, bind, induce,
    removeVertex, splitVertex, removeEdge,

    -- * Graph composition
    box,

    -- * Re-exporting standard functions
    toList
  ) where

import Algebra.Graph.Base
import Algebra.Graph.Fold
