-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.HigherKinded.Base
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module implements basic graph construction primitives for the core
-- type class 'Graph' defined in "Algebra.Graph.Classes".
--
-----------------------------------------------------------------------------
module Algebra.Graph.HigherKinded.Base (
    -- * The core type class
    Graph (..), empty, vertex, overlay,

    -- * Basic graph construction primitives
    vertices, overlays, connects, edge, edges, graph,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest
  ) where

import Control.Monad
import Data.Tree

import Algebra.Graph.HigherKinded.Classes

-- | Construct the graph comprising a given list of isolated vertices.
--
-- @
-- vertices []  == 'empty'
-- vertices [x] == 'vertex' x
-- @
vertices :: Graph g => [a] -> g a
vertices = overlays . map vertex

-- | Overlay a given list of graphs.
--
-- @
-- overlays []    == 'empty'
-- overlays [x]   == x
-- overlays [x,y] == 'overlay' x y
-- @
overlays :: Graph g => [g a] -> g a
overlays = msum

-- | Connect a given list of graphs.
--
-- @
-- connects []    == 'empty'
-- connects [x]   == x
-- connects [x,y] == 'connect' x y
-- @
connects :: Graph g => [g a] -> g a
connects = foldr connect empty

-- | Construct the graph comprising a single edge.
--
-- @
-- edge x y == 'connect' ('vertex' x) ('vertex' y)
-- @
edge :: Graph g => a -> a -> g a
edge x y = connect (vertex x) (vertex y)

-- | Construct the graph from a list of edges.
--
-- @
-- edges []      == 'empty'
-- edges [(x,y)] == 'edge' x y
-- @
edges :: Graph g => [(a, a)] -> g a
edges = overlays . map (uncurry edge)

-- | Construct the graph given a list of vertices @V@ and a list of edges @E@.
-- The resulting graph contains both the vertices @V@ and all the vertices
-- referred to by the edges @E@.
--
-- @
-- graph []  []      == 'empty'
-- graph [x] []      == 'vertex' x
-- graph []  [(x,y)] == 'edge' x y
-- @
graph :: Graph g => [a] -> [(a, a)] -> g a
graph vs es = overlay (vertices vs) (edges es)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf :: (Graph g, Eq (g a)) => g a -> g a -> Bool
isSubgraphOf x y = overlay x y == y

-- | The /path/ on a list of vertices.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'vertex' x
-- path [x,y] == 'edge' x y
-- @
path :: Graph g => [a] -> g a
path []  = empty
path [x] = vertex x
path xs  = edges $ zip xs (tail xs)

-- | The /circuit/ on a list of vertices.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'edge' x x
-- circuit [x,y] == 'edges' [(x,y), (y,x)]
-- @
circuit :: Graph g => [a] -> g a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
--
-- @
-- clique []      == 'empty'
-- clique [x]     == 'vertex' x
-- clique [x,y]   == 'edge' x y
-- clique [x,y,z] == 'edges' [(x,y), (x,z), (y,z)]
-- @
clique :: Graph g => [a] -> g a
clique = connects . map vertex

-- | The /biclique/ on a list of vertices.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- @
biclique :: Graph g => [a] -> [a] -> g a
biclique xs ys = connect (vertices xs) (vertices ys)

-- | The /star/ formed by a centre vertex and a list of leaves.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- @
star :: Graph g => a -> [a] -> g a
star x ys = connect (vertex x) (vertices ys)

-- | The /tree graph/ constructed from a given 'Tree' data structure.
tree :: Graph g => Tree a -> g a
tree (Node x f) = overlay (star x $ map rootLabel f) (forest f)

-- | The /forest graph/ constructed from a given 'Forest' data structure.
forest :: Graph g => Forest a -> g a
forest = overlays . map tree
