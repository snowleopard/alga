-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled
-- Copyright  : (c) Andrey Mokhov 2016-2020
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
    -- * Algebraic data type for edge-labelled graphs
    Graph (..), empty, vertex, edge, (-<), (>-), overlay, connect, vertices,
    edges, overlays,

    -- * Graph folding
    foldg, buildg,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, size, hasVertex, hasEdge, edgeLabel, vertexList, edgeList,
    vertexSet, edgeSet,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, replaceEdge, transpose, emap,
    induce, induceJust,

    -- * Relational operations
    closure, reflexiveClosure, symmetricClosure, transitiveClosure,

    -- * Types of edge-labelled graphs
    UnlabelledGraph, Automaton, Network,

    -- * Context
    Context (..), context
    ) where

import Data.Bifunctor
import Data.Monoid
import Data.String
import Control.DeepSeq
import GHC.Generics

import Algebra.Graph.Internal (List (..))
import Algebra.Graph.Label

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Set                            as Set
import qualified Data.Map                            as Map
import qualified GHC.Exts                            as Exts

-- | Edge-labelled graphs, where the type variable @e@ stands for edge labels.
-- For example, 'Graph' @Bool@ @a@ is isomorphic to unlabelled graphs defined in
-- the top-level module "Algebra.Graph.Graph", where @False@ and @True@ denote
-- the lack of and the existence of an unlabelled edge, respectively.
data Graph e a = Empty
               | Vertex a
               | Connect e (Graph e a) (Graph e a)
               deriving (Functor, Show, Generic)

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

instance IsString a => IsString (Graph e a) where
    fromString = Vertex . fromString

instance Bifunctor Graph where
    bimap f g = foldg Empty (Vertex . g) (Connect . f)

instance (NFData e, NFData a) => NFData (Graph e a) where
    rnf Empty           = ()
    rnf (Vertex  x    ) = rnf x
    rnf (Connect e x y) = e `seq` rnf x `seq` rnf y

-- TODO: This is a very inefficient implementation. Find a way to construct an
-- adjacency map directly, without building intermediate representations for all
-- subgraphs.
-- Extract the adjacency map of a graph.
toAdjacencyMap :: (Eq e, Monoid e, Ord a) => Graph e a -> AM.AdjacencyMap e a
toAdjacencyMap = foldg AM.empty AM.vertex AM.connect

-- Convert the adjacency map to a graph.
fromAdjacencyMap :: Monoid e => AM.AdjacencyMap e a -> Graph e a
fromAdjacencyMap = overlays . map go . Map.toList . AM.adjacencyMap
  where
    go (u, m) = overlay (vertex u) (edges [ (e, u, v) | (v, e) <- Map.toList m])

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, vertex and connect.
-- Complexity: /O(s)/ applications of the given functions. As an example, the
-- complexity of 'size' is /O(s)/, since 'const' and '+' have constant costs.
--
-- @
-- foldg 'empty'     'vertex'        'connect'             == 'id'
-- foldg 'empty'     'vertex'        ('fmap' 'flip' 'connect') == 'transpose'
-- foldg 1         ('const' 1)     ('const' (+))         == 'size'
-- foldg True      ('const' False) ('const' (&&))        == 'isEmpty'
-- foldg False     (== x)        ('const' (||))        == 'hasVertex' x
-- foldg Set.'Set.empty' Set.'Set.singleton' ('const' Set.'Set.union')   == 'vertexSet'
-- @
foldg :: b -> (a -> b) -> (e -> b -> b -> b) -> Graph e a -> b
foldg e v c = go
  where
    go Empty           = e
    go (Vertex    x  ) = v x
    go (Connect e x y) = c e (go x) (go y)

-- | Build a graph given an interpretation of the three graph construction
-- primitives 'empty', 'vertex' and 'connect', in this order. See examples for
-- further clarification.
--
-- @
-- buildg f                                               == f 'empty' 'vertex' 'connect'
-- buildg (\\e _ _ -> e)                                   == 'empty'
-- buildg (\\_ v _ -> v x)                                 == 'vertex' x
-- buildg (\\e v c -> c l ('foldg' e v c x) ('foldg' e v c y)) == 'connect' l x y
-- buildg (\\e v c -> 'foldr' (c 'zero') e ('map' v xs))         == 'vertices' xs
-- buildg (\\e v c -> 'foldg' e v ('flip' . c) g)              == 'transpose' g
-- 'foldg' e v c (buildg f)                                 == f e v c
-- @
buildg :: (forall r. r -> (a -> r) -> (e -> r -> r -> r) -> r) -> Graph e a
buildg f = f Empty Vertex Connect

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
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'Algebra.Graph.ToGraph.vertexCount' empty == 0
-- 'Algebra.Graph.ToGraph.edgeCount'   empty == 0
-- @
empty :: Graph e a
empty = Empty

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex y) == (x == y)
-- 'Algebra.Graph.ToGraph.vertexCount' (vertex x) == 1
-- 'Algebra.Graph.ToGraph.edgeCount'   (vertex x) == 0
-- @
vertex :: a -> Graph e a
vertex = Vertex

-- | Construct the graph comprising /a single labelled edge/.
--
-- @
-- edge e    x y              == 'connect' e ('vertex' x) ('vertex' y)
-- edge 'zero' x y              == 'vertices' [x,y]
-- 'hasEdge'   x y (edge e x y) == (e /= 'zero')
-- 'edgeLabel' x y (edge e x y) == e
-- 'Algebra.Graph.ToGraph.edgeCount'     (edge e x y) == if e == 'zero' then 0 else 1
-- 'Algebra.Graph.ToGraph.vertexCount'   (edge e 1 1) == 1
-- 'Algebra.Graph.ToGraph.vertexCount'   (edge e 1 2) == 2
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

-- | /Overlay/ two graphs. An alias for 'Connect' 'zero'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'Algebra.Graph.ToGraph.vertexCount' (overlay x y) >= 'Algebra.Graph.ToGraph.vertexCount' x
-- 'Algebra.Graph.ToGraph.vertexCount' (overlay x y) <= 'Algebra.Graph.ToGraph.vertexCount' x + 'Algebra.Graph.ToGraph.vertexCount' y
-- 'Algebra.Graph.ToGraph.edgeCount'   (overlay x y) >= 'Algebra.Graph.ToGraph.edgeCount' x
-- 'Algebra.Graph.ToGraph.edgeCount'   (overlay x y) <= 'Algebra.Graph.ToGraph.edgeCount' x   + 'Algebra.Graph.ToGraph.edgeCount' y
-- 'Algebra.Graph.ToGraph.vertexCount' (overlay 1 2) == 2
-- 'Algebra.Graph.ToGraph.edgeCount'   (overlay 1 2) == 0
-- @
--
-- Note: 'overlay' composes edges in parallel using the operator '<+>' with
-- 'zero' acting as the identity:
--
-- @
-- 'edgeLabel' x y $ overlay ('edge' e x y) ('edge' 'zero' x y) == e
-- 'edgeLabel' x y $ overlay ('edge' e x y) ('edge' f    x y) == e '<+>' f
-- @
--
-- Furthermore, when applied to transitive graphs, 'overlay' composes edges in
-- sequence using the operator '<.>' with 'one' acting as the identity:
--
-- @
-- 'edgeLabel' x z $ 'transitiveClosure' (overlay ('edge' e x y) ('edge' 'one' y z)) == e
-- 'edgeLabel' x z $ 'transitiveClosure' (overlay ('edge' e x y) ('edge' f   y z)) == e '<.>' f
-- @
overlay :: Monoid e => Graph e a -> Graph e a -> Graph e a
overlay = connect zero

-- | /Connect/ two graphs with edges labelled by a given label. An alias for
-- 'Connect'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'isEmpty'     (connect e x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect e x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'Algebra.Graph.ToGraph.vertexCount' (connect e x y) >= 'Algebra.Graph.ToGraph.vertexCount' x
-- 'Algebra.Graph.ToGraph.vertexCount' (connect e x y) <= 'Algebra.Graph.ToGraph.vertexCount' x + 'Algebra.Graph.ToGraph.vertexCount' y
-- 'Algebra.Graph.ToGraph.edgeCount'   (connect e x y) <= 'Algebra.Graph.ToGraph.vertexCount' x * 'Algebra.Graph.ToGraph.vertexCount' y + 'Algebra.Graph.ToGraph.edgeCount' x + 'Algebra.Graph.ToGraph.edgeCount' y
-- 'Algebra.Graph.ToGraph.vertexCount' (connect e 1 2) == 2
-- 'Algebra.Graph.ToGraph.edgeCount'   (connect e 1 2) == if e == 'zero' then 0 else 1
-- @
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
-- 'Algebra.Graph.ToGraph.vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'Algebra.Graph.ToGraph.vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Monoid e => [a] -> Graph e a
vertices = overlays . map vertex

-- | Construct the graph from a list of labelled edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []        == 'empty'
-- edges [(e,x,y)] == 'edge' e x y
-- edges           == 'overlays' . 'map' (\\(e, x, y) -> 'edge' e x y)
-- @
edges :: Monoid e => [(e, a, a)] -> Graph e a
edges = overlays . map (\(e, x, y) -> edge e x y)

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: Monoid e => [Graph e a] -> Graph e a
overlays = foldr overlay empty

-- | Check if a graph is empty.
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
-- size x             >= 'Algebra.Graph.ToGraph.vertexCount' x
-- @
size :: Graph e a -> Int
size = foldg 1 (const 1) (const (+))

-- | Check if a graph contains a given vertex.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' y)       == (x == y)
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

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Ord a => Graph e a -> [a]
vertexList = Set.toAscList . vertexSet

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

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: Ord a => Graph e a -> Set.Set a
vertexSet = foldg Set.empty Set.singleton (const Set.union)

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

-- | Remove an edge from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x y ('edge' e x y)     == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: (Eq a, Eq e, Monoid e) => a -> a -> Graph e a -> Graph e a
removeEdge s t = filterContext s (/=s) (/=t)

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

-- | Replace an edge from a given graph. If it doesn't exist, it will be created.
-- Complexity: /O(log(n))/ time.
--
-- @
-- replaceEdge e x y z                 == 'overlay' (removeEdge x y z) ('edge' e x y)
-- replaceEdge e x y ('edge' f x y)      == 'edge' e x y
-- 'edgeLabel' x y (replaceEdge e x y z) == e
-- @
replaceEdge :: (Eq e, Monoid e, Ord a) => e -> a -> a -> Graph e a -> Graph e a
replaceEdge e x y = overlay (edge e x y) . removeEdge x y

-- | Transpose a given graph.
-- Complexity: /O(s)/ time, memory and size.
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
-- Complexity: /O(s)/ time, memory and size.
--
-- The function @h@ is required to be a /homomorphism/ on the underlying type of
-- labels @e@. At the very least it must preserve 'zero' and '<+>':
--
-- @
-- h 'zero'      == 'zero'
-- h x '<+>' h y == h (x '<+>' y)
-- @
--
-- If @e@ is also a semiring, then @h@ must also preserve the multiplicative
-- structure:
--
-- @
-- h 'one'       == 'one'
-- h x '<.>' h y == h (x '<.>' y)
-- @
--
-- If the above requirements hold, then the implementation provides the
-- following guarantees.
--
-- @
-- emap h 'empty'           == 'empty'
-- emap h ('vertex' x)      == 'vertex' x
-- emap h ('edge' e x y)    == 'edge' (h e) x y
-- emap h ('overlay' x y)   == 'overlay' (emap h x) (emap h y)
-- emap h ('connect' e x y) == 'connect' (h e) (emap h x) (emap h y)
-- emap 'id'                == 'id'
-- emap g . emap h        == emap (g . h)
-- @
emap :: (e -> f) -> Graph e a -> Graph f a
emap f = foldg Empty Vertex (Connect . f)

-- TODO: Implement via 'induceJust' to reduce code duplication.
-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- constant time.
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

-- | Construct the /induced subgraph/ of a given graph by removing the vertices
-- that are 'Nothing'.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- induceJust ('vertex' 'Nothing')                               == 'empty'
-- induceJust ('edge' ('Just' x) 'Nothing')                        == 'vertex' x
-- induceJust . 'fmap' 'Just'                                    == 'id'
-- induceJust . 'fmap' (\\x -> if p x then 'Just' x else 'Nothing') == 'induce' p
-- @
induceJust :: Graph e (Maybe a) -> Graph e a
induceJust = foldg Empty (maybe Empty Vertex) c
  where
    c _ x     Empty = x -- Constant folding to get rid of Empty leaves
    c _ Empty y     = y
    c e x     y     = Connect e x y

-- | Compute the /reflexive and transitive closure/ of a graph over the
-- underlying star semiring using the Warshall-Floyd-Kleene algorithm.
--
-- @
-- closure 'empty'         == 'empty'
-- closure ('vertex' x)    == 'edge' 'one' x x
-- closure ('edge' e x x)  == 'edge' 'one' x x
-- closure ('edge' e x y)  == 'edges' [('one',x,x), (e,x,y), ('one',y,y)]
-- closure               == 'reflexiveClosure' . 'transitiveClosure'
-- closure               == 'transitiveClosure' . 'reflexiveClosure'
-- closure . closure     == closure
-- 'Algebra.Graph.ToGraph.postSet' x (closure y) == Set.'Set.fromList' ('Algebra.Graph.ToGraph.reachable' x y)
-- @
closure :: (Eq e, Ord a, StarSemiring e) => Graph e a -> Graph e a
closure = fromAdjacencyMap . AM.closure . toAdjacencyMap

-- | Compute the /reflexive closure/ of a graph over the underlying semiring by
-- adding a self-loop of weight 'one' to every vertex.
-- Complexity: /O(n * log(n))/ time.
--
-- @
-- reflexiveClosure 'empty'              == 'empty'
-- reflexiveClosure ('vertex' x)         == 'edge' 'one' x x
-- reflexiveClosure ('edge' e x x)       == 'edge' 'one' x x
-- reflexiveClosure ('edge' e x y)       == 'edges' [('one',x,x), (e,x,y), ('one',y,y)]
-- reflexiveClosure . reflexiveClosure == reflexiveClosure
-- @
reflexiveClosure :: (Ord a, Semiring e) => Graph e a -> Graph e a
reflexiveClosure x = overlay x $ edges [ (one, v, v) | v <- vertexList x ]

-- | Compute the /symmetric closure/ of a graph by overlaying it with its own
-- transpose.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- symmetricClosure 'empty'              == 'empty'
-- symmetricClosure ('vertex' x)         == 'vertex' x
-- symmetricClosure ('edge' e x y)       == 'edges' [(e,x,y), (e,y,x)]
-- symmetricClosure x                  == 'overlay' x ('transpose' x)
-- symmetricClosure . symmetricClosure == symmetricClosure
-- @
symmetricClosure :: Monoid e => Graph e a -> Graph e a
symmetricClosure m = overlay m (transpose m)

-- | Compute the /transitive closure/ of a graph over the underlying star
-- semiring using a modified version of the Warshall-Floyd-Kleene algorithm,
-- which omits the reflexivity step.
--
-- @
-- transitiveClosure 'empty'               == 'empty'
-- transitiveClosure ('vertex' x)          == 'vertex' x
-- transitiveClosure ('edge' e x y)        == 'edge' e x y
-- transitiveClosure . transitiveClosure == transitiveClosure
-- @
transitiveClosure :: (Eq e, Ord a, StarSemiring e) => Graph e a -> Graph e a
transitiveClosure = fromAdjacencyMap . AM.transitiveClosure . toAdjacencyMap

-- | A type synonym for /unlabelled graphs/.
type UnlabelledGraph a = Graph Any a

-- | A type synonym for /automata/ or /labelled transition systems/.
type Automaton a s = Graph (RegularExpression a) s

-- | A /network/ is a graph whose edges are labelled with distances.
type Network e a = Graph (Distance e) a

-- Filter vertices in a subgraph context.
filterContext :: (Eq a, Eq e, Monoid e) => a -> (a -> Bool) -> (a -> Bool) -> Graph e a -> Graph e a
filterContext s i o g = maybe g go $ context (==s) g
  where
    go (Context is os) = overlays [ vertex s
                                  , induce (/=s) g
                                  , edges [ (e, v, s) | (e, v) <- is, i v ]
                                  , edges [ (e, s, v) | (e, v) <- os, o v ] ]

-- The /focus/ of a graph expression is a flattened represenentation of the
-- subgraph under focus, its context, as well as the list of all encountered
-- vertices. See 'removeEdge' for a use-case example.
data Focus e a = Focus
    { ok :: Bool        -- ^ True if focus on the specified subgraph is obtained.
    , is :: List (e, a) -- ^ Inputs into the focused subgraph.
    , os :: List (e, a) -- ^ Outputs out of the focused subgraph.
    , vs :: List a    } -- ^ All vertices (leaves) of the graph expression.

-- Focus on the 'empty' graph.
emptyFocus :: Focus e a
emptyFocus = Focus False mempty mempty mempty

-- | Focus on the graph with a single vertex, given a predicate indicating
-- whether the vertex is of interest.
vertexFocus :: (a -> Bool) -> a -> Focus e a
vertexFocus f x = Focus (f x) mempty mempty (pure x)

-- | Connect two foci.
connectFoci :: (Eq e, Monoid e) => e -> Focus e a -> Focus e a -> Focus e a
connectFoci e x y
    | e == mempty = Focus (ok x || ok y) (is x <> is y) (os x <> os y) (vs x <> vs y)
    | otherwise   = Focus (ok x || ok y) (xs   <> is y) (os x <> ys  ) (vs x <> vs y)
  where
    xs = if ok y then fmap (e,) (vs x) else is x
    ys = if ok x then fmap (e,) (vs y) else os y

-- | 'Focus' on a specified subgraph.
focus :: (Eq e, Monoid e) => (a -> Bool) -> Graph e a -> Focus e a
focus f = foldg emptyFocus (vertexFocus f) connectFoci

-- | The 'Context' of a subgraph comprises its 'inputs' and 'outputs', i.e. all
-- the vertices that are connected to the subgraph's vertices (along with the
-- corresponding edge labels). Note that inputs and outputs can belong to the
-- subgraph itself. In general, there are no guarantees on the order of vertices
-- in 'inputs' and 'outputs'; furthermore, there may be repetitions.
data Context e a = Context { inputs :: [(e, a)], outputs :: [(e, a)] }
    deriving (Eq, Show)

-- | Extract the 'Context' of a subgraph specified by a given predicate. Returns
-- @Nothing@ if the specified subgraph is empty.
--
-- @
-- context ('const' False) x                   == Nothing
-- context (== 1)        ('edge' e 1 2)        == if e == 'zero' then Just ('Context' [] []) else Just ('Context' [     ] [(e,2)])
-- context (== 2)        ('edge' e 1 2)        == if e == 'zero' then Just ('Context' [] []) else Just ('Context' [(e,1)] [     ])
-- context ('const' True ) ('edge' e 1 2)        == if e == 'zero' then Just ('Context' [] []) else Just ('Context' [(e,1)] [(e,2)])
-- context (== 4)        (3 * 1 * 4 * 1 * 5) == Just ('Context' [('one',3), ('one',1)] [('one',1), ('one',5)])
-- @
context :: (Eq e, Monoid e) => (a -> Bool) -> Graph e a -> Maybe (Context e a)
context p g | ok f      = Just $ Context (Exts.toList $ is f) (Exts.toList $ os f)
            | otherwise = Nothing
  where
    f = focus p g
