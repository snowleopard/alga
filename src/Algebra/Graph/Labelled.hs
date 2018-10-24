{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides a minimal and experimental implementation of algebraic
-- graphs with edge labels. The API will be expanded in the next release.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled (
    -- * Algebraic data type for edge-labeleld graphs
    Graph (..), empty, vertex, edge, overlay, connect, edges, overlays,
    (-<), (>-),

    -- * Relations on graphs
    isSubgraphOf,

    -- * Operations
    edgeLabel, emap,

    -- * Types of edge-labelled graphs
    UnlabelledGraph, Automaton, Network
  ) where

import Prelude ()
import Prelude.Compat

import Data.Monoid (Any (..))

import Algebra.Graph.Label
import qualified Algebra.Graph.Class                 as C
import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Algebra.Graph.ToGraph               as U

-- | Edge-labelled graphs, where the type variable @e@ stands for edge labels.
-- For example, @Graph Bool a@ is isomorphic to unlabelled graphs defined in
-- the top-level module "Algebra.Graph.Graph", where @False@ and @True@ denote
-- the lack of and the existence of an unlabelled edge, respectively.
data Graph e a = Empty
               | Vertex a
               | Connect e (Graph e a) (Graph e a)
               deriving (Functor, Show)

instance (Ord a, Eq e, Monoid e) => Eq (Graph e a) where
    x == y = toAdjacencyMap x == toAdjacencyMap y

-- | Extract the adjacency map of a graph.
toAdjacencyMap :: (Ord a, Monoid e) => Graph e a -> AM.AdjacencyMap e a
toAdjacencyMap = foldg AM.empty AM.vertex AM.connect

instance Dioid e => C.Graph (Graph e a) where
    type Vertex (Graph e a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = overlay
    connect = connect one

instance U.ToGraph (Graph Bool a) where
    type ToVertex (Graph Bool a) = a
    foldg e v o c = foldg e v (\x -> if x then c else o)

foldg :: b -> (a -> b) -> (e -> b -> b -> b) -> Graph e a -> b
foldg e v c = go
  where
    go Empty           = e
    go (Vertex    x  ) = v x
    go (Connect e x y) = c e (go x) (go y)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- @
isSubgraphOf :: (Eq e, Monoid e, Ord a) => Graph e a -> Graph e a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Construct the /empty graph/. An alias for the constructor 'Empty'.
-- Complexity: /O(1)/ time, memory and size.
empty :: Graph e a
empty = Empty

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
-- Complexity: /O(1)/ time, memory and size.
vertex :: a -> Graph e a
vertex = Vertex

-- | Construct the graph comprising /a single labelled edge/.
-- Complexity: /O(1)/ time, memory and size.
edge :: e -> a -> a -> Graph e a
edge e x y = connect e (vertex x) (vertex y)

-- | Construct the graph from a list of labelled edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
edges :: Monoid e => [(e, a, a)] -> Graph e a
edges = overlays . map (\(e, x, y) -> edge e x y)

-- | /Overlay/ two graphs. An alias for 'Connect' 'zero'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
overlay :: Monoid e => Graph e a -> Graph e a -> Graph e a
overlay = connect zero

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
overlays :: Monoid e => [Graph e a] -> Graph e a
overlays = foldr overlay empty

-- | /Connect/ two graphs with edges labelled by a given label. An alias for
-- 'Connect'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
connect :: e -> Graph e a -> Graph e a -> Graph e a
connect = Connect

-- | The left-hand part of a convenient ternary-ish operator @x -\<e\>- y@ for
-- creating labelled edges. For example:
--
-- @
-- z = x -\<2\>- y
-- @
(-<) :: a -> e -> (a, e)
x -< e = (x, e)

-- | The right-hand part of a convenient ternary-ish operator @x -\<e\>- y@ for
-- creating labelled edges. For example:
--
-- @
-- z = x -\<2\>- y
-- @
(>-) :: (a, e) -> a -> Graph e a
(x, e) >- y = edge e x y

infixl 5 -<
infixl 5 >-

-- | Extract the label of a specified edge from a graph.
edgeLabel :: (Eq a, Monoid e) => a -> a -> Graph e a -> e
edgeLabel s t g = let (res, _, _) = foldg e v c g in res
  where
    e                                         = (zero               , False   , False   )
    v x                                       = (zero               , x == s  , x == t  )
    c l (l1, s1, t1) (l2, s2, t2) | s1 && t2  = (mconcat [l1, l, l2], s1 || s2, t1 || t2)
                                  | otherwise = (mconcat [l1,    l2], s1 || s2, t1 || t2)

-- | Transform a graph by applying a function to each of its edge labels.
-- Complexity: /O((n + m) * log(n))/ time.
emap :: (e -> f) -> Graph e a -> Graph f a
emap f = foldg Empty Vertex (Connect . f)

-- | A type synonym for /unlabelled graphs/.
type UnlabelledGraph a = Graph Any a

-- | A type synonym for /automata/ or /labelled transition systems/.
type Automaton a s = Graph (RegularExpression a) s

-- | A /network/ is a graph whose edges are labelled with distances.
type Network e a = Graph (Distance e) a
