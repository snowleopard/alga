-----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Graph
-- Copyright   :  (c) Andrey Mokhov 2016-2017
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- A library for algebraic construction and manipulation of graphs in Haskell.
-- See <https://github.com/snowleopard/alga-paper this paper> for the motivation
-- behind the library, the underlying theory and implementation details.
--
-----------------------------------------------------------------------------
module Algebra.Graph (
    -- * The core type class
    Graph (..),

    -- * Basic graph construction primitives
    vertices, overlays, connects, edge, edges, graph,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest
  ) where

import Data.Tree

import Algebra.Graph.Classes

-- | Construct the graph comprising a given list of isolated vertices.
--
-- > vertices []  == empty
-- > vertices [x] == vertex x
vertices :: Graph g => [Vertex g] -> g
vertices = overlays . map vertex

-- | Overlay a given list of graphs.
--
-- > overlays []     == empty
-- > overlays [x]    == x
-- > overlays [x, y] == overlay x y
overlays :: Graph g => [g] -> g
overlays = foldr overlay empty

-- | Connect a given list of graphs.
--
-- > connects []     == empty
-- > connects [x]    == x
-- > connects [x, y] == connect x y
connects :: Graph g => [g] -> g
connects = foldr connect empty

-- | Construct the graph comprising a single edge.
--
-- > edge x y == connect (vertex x) (vertex y)
edge :: Graph g => Vertex g -> Vertex g -> g
edge x y = connect (vertex x) (vertex y)

-- | Construct the graph from a list of edges.
--
-- > edges []       == empty
-- > edges [(x, y)] == edge x y
edges :: Graph g => [(Vertex g, Vertex g)] -> g
edges = overlays . map (uncurry edge)

-- | Construct the graph given a list of vertices @V@ and a list of edges @E@.
-- The resulting graph contains both the vertices @V@ and all the vertices
-- referred to by the edges @E@.
--
-- > graph []  []       == empty
-- > graph [x] []       == vertex x
-- > graph []  [(x, y)] == edge x y
graph :: Graph g => [Vertex g] -> [(Vertex g, Vertex g)] -> g
graph vs es = overlay (vertices vs) (edges es)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
isSubgraphOf :: (Graph g, Eq g) => g -> g -> Bool
isSubgraphOf x y = overlay x y == y

-- | The /path/ on a list of vertices.
--
-- > path []     == empty
-- > path [x]    == vertex x
-- > path [x, y] == edge x y
path :: Graph g => [Vertex g] -> g
path []  = empty
path [x] = vertex x
path xs  = edges $ zip xs (tail xs)

-- | The /circuit/ on a list of vertices.
--
-- > circuit []     == empty
-- > circuit [x]    == edge x x
-- > circuit [x, y] == edges [(x, y), (y, x)]
circuit :: Graph g => [Vertex g] -> g
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
--
-- > clique []        == empty
-- > clique [x]       == vertex x
-- > clique [x, y]    == edge x y
-- > clique [x, y, z] == edges [(x, y), (x, z), (y, z)]
clique :: Graph g => [Vertex g] -> g
clique = connects . map vertex

-- | The /biclique/ on a list of vertices.
--
-- > biclique []       []       == empty
-- > biclique [x]      []       == vertex x
-- > biclique []       [y]      == vertex y
-- > biclique [x1, x2] [y1, y2] == edges [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]
biclique :: Graph g => [Vertex g] -> [Vertex g] -> g
biclique xs ys = connect (vertices xs) (vertices ys)

-- | The /star/ formed by a centre vertex and a list of leaves.
--
-- > star x []     == vertex x
-- > star x [y]    == edge x y
-- > star x [y, z] == edges [(x, y), (x, z)]
star :: Graph g => Vertex g -> [Vertex g] -> g
star x ys = connect (vertex x) (vertices ys)

-- | The /tree/ graph constructed from a given 'Tree' data structure.
tree :: Graph g => Tree (Vertex g) -> g
tree (Node x f) = overlay (star x $ map rootLabel f) (forest f)

-- | The /forest/ graph constructed from a given 'Forest' data structure.
forest :: Graph g => Forest (Vertex g) -> g
forest = overlays . map tree
