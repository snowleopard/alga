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
    Graph (..), empty, vertex, edge, (-<), (>-), overlay, connect, vertices,
    edges, overlays,

    -- * Graph folding
    foldg,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, size, hasVertex, hasEdge, edgeLabel, edgeList, edgeSet,

    -- * Graph transformation
    removeVertex, replaceVertex,
    -- removeEdge, replaceEdge,
    transpose,
    --  gmap,
    emap, induce,

    -- * Types of edge-labelled graphs
    UnlabelledGraph, Automaton, Network
    ) where

import Prelude ()
import Prelude.Compat

import Data.Monoid (Any (..))

import Algebra.Graph.Label
import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Set                            as Set

-- | Edge-labelled graphs, where the type variable @e@ stands for edge labels.
-- For example, 'Graph' @Bool@ @a@ is isomorphic to unlabelled graphs defined in
-- the top-level module "Algebra.Graph.Graph", where @False@ and @True@ denote
-- the lack of and the existence of an unlabelled edge, respectively.
data Graph e a = Empty
               | Vertex a
               | Connect e (Graph e a) (Graph e a)
               deriving (Functor, Show)

instance (Eq e, Monoid e, Ord a) => Eq (Graph e a) where
    x == y = toAdjacencyMap x == toAdjacencyMap y

instance (Eq e, Monoid e, Ord a, Ord e) => Ord (Graph e a) where
    compare x y = compare (toAdjacencyMap x) (toAdjacencyMap y)

-- | __Note:__ this does not satisfy the usual ring laws; see 'Graph'
-- for more details.
instance (Ord a, Num a, Dioid e) => Num (Graph e a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect one
    signum      = const empty
    abs         = id
    negate      = id

-- TODO: This is a very inefficient implementation. Find a way to construct an
-- adjacency map directly, without building intermediate representations for all
-- subgraphs.
-- Extract the adjacency map of a graph.
toAdjacencyMap :: (Eq e, Monoid e, Ord a) => Graph e a -> AM.AdjacencyMap e a
toAdjacencyMap = foldg AM.empty AM.vertex AM.connect

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, vertex and connect.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg 'empty' 'vertex'        'connect'             == 'id'
-- foldg 'empty' 'vertex'        ('fmap' 'flip' 'connect') == 'transpose'
-- foldg 1     ('const' 1)     ('const' (+))         == 'size'
-- foldg True  ('const' False) ('const' (&&))        == 'isEmpty'
-- foldg False (== x)        ('const' (||))        == 'hasVertex' x
-- @
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
-- isSubgraphOf 'empty'         x             ==  True
-- isSubgraphOf ('vertex' x)    'empty'         ==  False
-- isSubgraphOf x             ('overlay' x y) ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: (Eq e, Monoid e, Ord a) => Graph e a -> Graph e a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Construct the /empty graph/. An alias for the constructor 'Empty'.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: Graph e a
empty = Empty

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: a -> Graph e a
vertex = Vertex

-- | Construct the graph comprising /a single labelled edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge e    x y              == 'connect' e ('vertex' x) ('vertex' y)
-- edge 'zero' x y              == 'vertices' [x,y]
-- 'hasEdge'   x y (edge e x y) == (e /= 'zero')
-- 'edgeLabel' x y (edge e x y) == e
-- 'edgeCount'     (edge e x y) == if e == 'zero' then 0 else 1
-- 'vertexCount'   (edge e 1 1) == 1
-- 'vertexCount'   (edge e 1 2) == 2
-- @
edge :: e -> a -> a -> Graph e a
edge e x y = connect e (vertex x) (vertex y)

-- | The left-hand part of a convenient ternary-ish operator @x-\<e\>-y@ for
-- creating labelled edges.
--
-- @
-- x -\<e\>- y == 'edge' e x y
-- @
(-<) :: a -> e -> (a, e)
g -< e = (g, e)

-- | The right-hand part of a convenient ternary-ish operator @x-\<e\>-y@ for
-- creating labelled edges.
--
-- @
-- x -\<e\>- y == 'edge' e x y
-- @
(>-) :: (a, e) -> a -> Graph e a
(x, e) >- y = edge e x y

infixl 5 -<
infixl 5 >-

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

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Monoid e => [a] -> Graph e a
vertices = overlays . map vertex

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                         == True
-- isEmpty ('overlay' 'empty' 'empty')         == True
-- isEmpty ('vertex' x)                    == False
-- isEmpty ('removeVertex' x $ 'vertex' x)   == True
-- isEmpty ('removeEdge' x y $ 'edge' e x y) == False
-- @
isEmpty :: Graph e a -> Bool
isEmpty = foldg True (const False) (const (&&))

-- | The /size/ of a graph, i.e. the number of leaves of the expression
-- including 'empty' leaves.
-- Complexity: /O(s)/ time.
--
-- @
-- size 'empty'         == 1
-- size ('vertex' x)    == 1
-- size ('overlay' x y) == size x + size y
-- size ('connect' x y) == size x + size y
-- size x             >= 1
-- size x             >= 'vertexCount' x
-- @
size :: Graph e a -> Int
size = foldg 1 (const 1) (const (+))

-- | Check if a graph contains a given vertex.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Eq a => a -> Graph e a -> Bool
hasVertex x = foldg False (==x) (const (||))

-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' e x y)     == (e /= 'zero')
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'not' . 'null' . 'filter' (\\(_,ex,ey) -> ex == x && ey == y) . 'edgeList'
-- @
hasEdge :: (Eq e, Monoid e, Ord a) => a -> a -> Graph e a -> Bool
hasEdge x y = (/= zero) . edgeLabel x y

-- | Extract the label of a specified edge from a graph.
edgeLabel :: (Eq a, Monoid e) => a -> a -> Graph e a -> e
edgeLabel s t g = let (res, _, _) = foldg e v c g in res
  where
    e                                         = (zero               , False   , False   )
    v x                                       = (zero               , x == s  , x == t  )
    c l (l1, s1, t1) (l2, s2, t2) | s1 && t2  = (mconcat [l1, l, l2], s1 || s2, t1 || t2)
                                  | otherwise = (mconcat [l1,    l2], s1 || s2, t1 || t2)

-- | The list of edges of a graph, sorted lexicographically with respect to
-- pairs of connected vertices (i.e. edge-labels are ignored when sorting).
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'        == []
-- edgeList ('vertex' x)   == []
-- edgeList ('edge' e x y) == if e == 'zero' then [] else [(e,x,y)]
-- @
edgeList :: (Eq e, Monoid e, Ord a) => Graph e a -> [(e, a, a)]
edgeList = AM.edgeList . toAdjacencyMap

-- | The set of edges of a given graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'        == Set.'Set.empty'
-- edgeSet ('vertex' x)   == Set.'Set.empty'
-- edgeSet ('edge' e x y) == if e == 'zero' then Set.'Set.empty' else Set.'Set.singleton' (e,x,y)
-- @
edgeSet :: (Eq e, Monoid e, Ord a) => Graph e a -> Set.Set (e, a, a)
edgeSet = Set.fromAscList . edgeList

-- | Remove a vertex from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' e x x)     == 'empty'
-- removeVertex 1 ('edge' e 1 2)     == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Eq a => a -> Graph e a -> Graph e a
removeVertex x = induce (/= x)

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'fmap' (\\v -> if v == x then y else v)
-- @
replaceVertex :: Eq a => a -> a -> Graph e a -> Graph e a
replaceVertex u v = fmap $ \w -> if w == u then v else w

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose 'empty'        == 'empty'
-- transpose ('vertex' x)   == 'vertex' x
-- transpose ('edge' e x y) == 'edge' e y x
-- transpose . transpose  == id
-- @
transpose :: Graph e a -> Graph e a
transpose = foldg empty vertex (fmap flip connect)

-- | Transform a graph by applying a function to each of its edge labels.
-- Complexity: /O((n + m) * log(n))/ time.
emap :: (e -> f) -> Graph e a -> Graph f a
emap f = foldg Empty Vertex (Connect . f)

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> Graph e a -> Graph e a
induce p = foldg Empty (\x -> if p x then Vertex x else Empty) c
  where
    c _ x     Empty = x -- Constant folding to get rid of Empty leaves
    c _ Empty y     = y
    c e x     y     = Connect e x y

-- | A type synonym for /unlabelled graphs/.
type UnlabelledGraph a = Graph Any a

-- | A type synonym for /automata/ or /labelled transition systems/.
type Automaton a s = Graph (RegularExpression a) s

-- | A /network/ is a graph whose edges are labelled with distances.
type Network e a = Graph (Distance e) a
