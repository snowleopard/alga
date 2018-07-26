{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Fold
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'Fold' data type -- the Boehm-Berarducci encoding of
-- algebraic graphs, which is used for generalised graph folding and for the
-- implementation of polymorphic graph construction and transformation algorithms.
-- 'Fold' is an instance of type classes defined in modules "Algebra.Graph.Class"
-- and "Algebra.Graph.HigherKinded.Class", which can be used for polymorphic
-- graph construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Fold (
    -- * Boehm-Berarducci encoding of algebraic graphs
    Fold,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Graph folding
    foldg,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, vertexIntSet, edgeSet, adjacencyList,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, starTranspose,

    -- * Graph transformation
    removeVertex, removeEdge, transpose, induce, simplify,
  ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Alternative, liftA2)
import Control.Monad.Compat (MonadPlus (..), ap)
import Data.Function

import Control.DeepSeq (NFData (..))

import Algebra.Graph.ToGraph (ToGraph, ToVertex, toGraph)

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.ToGraph      as T
import qualified Control.Applicative        as Ap
import qualified Data.IntSet                as IntSet
import qualified Data.Set                   as Set

{-| The 'Fold' data type is the Boehm-Berarducci encoding of the core graph
construction primitives 'empty', 'vertex', 'overlay' and 'connect'. We define a
'Num' instance as a convenient notation for working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show (empty     :: Fold Int) == "empty"
show (1         :: Fold Int) == "vertex 1"
show (1 + 2     :: Fold Int) == "vertices [1,2]"
show (1 * 2     :: Fold Int) == "edge 1 2"
show (1 * 2 * 3 :: Fold Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: Fold Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance is currently implemented using the 'AM.AdjacencyMap' as the
/canonical graph representation/ and satisfies all axioms of algebraic graphs:

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is associative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ will
denote the number of vertices in the graph, /m/ will denote the number of
edges in the graph, and /s/ will denote the /size/ of the corresponding
graph expression. For example, if g is a 'Fold' then /n/, /m/ and /s/ can be
computed as follows:

@n == 'vertexCount' g
m == 'edgeCount' g
s == 'size' g@

Note that 'size' is slightly different from the 'length' method of the
'Foldable' type class, as the latter does not count 'empty' leaves of the
expression:

@'length' 'empty'           == 0
'size'   'empty'           == 1
'length' ('vertex' x)      == 1
'size'   ('vertex' x)      == 1
'length' ('empty' + 'empty') == 0
'size'   ('empty' + 'empty') == 2@

The 'size' of any graph is positive, and the difference @('size' g - 'length' g)@
corresponds to the number of occurrences of 'empty' in an expression @g@.

Converting a 'Fold' to the corresponding 'AM.AdjacencyMap' takes /O(s + m * log(m))/
time and /O(s + m)/ memory. This is also the complexity of the graph equality test,
because it is currently implemented by converting graph expressions to canonical
representations based on adjacency maps.
-}
newtype Fold a = Fold { runFold :: forall b. b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b }

instance (Ord a, Show a) => Show (Fold a) where
    show = show . foldg AM.empty AM.vertex AM.overlay AM.connect

instance Ord a => Eq (Fold a) where
    x == y = T.adjacencyMap x == T.adjacencyMap y

instance NFData a => NFData (Fold a) where
    rnf = foldg () rnf seq seq

instance Num a => Num (Fold a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Functor Fold where
    fmap f = foldg empty (vertex . f) overlay connect

instance Applicative Fold where
    pure  = vertex
    (<*>) = ap

instance Alternative Fold where
    empty = empty
    (<|>) = overlay

instance MonadPlus Fold where
    mzero = empty
    mplus = overlay

instance Monad Fold where
    return = vertex
    g >>=f = foldg empty f overlay connect g

instance Foldable Fold where
    foldMap f = foldg mempty f mappend mappend

instance Traversable Fold where
    traverse f = foldg (pure empty) (fmap vertex . f) (liftA2 overlay) (liftA2 connect)

instance ToGraph (Fold a) where
    type ToVertex (Fold a) = a
    foldg = foldg

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- 'size'        empty == 1
-- @
empty :: Fold a
empty = Fold $ \e _ _ _ -> e

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- 'size'        (vertex x) == 1
-- @
vertex :: a -> Fold a
vertex x = Fold $ \_ v _ _ -> v x

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: a -> a -> Fold a
edge x y = Fold $ \_ v _ c -> v x `c` v y

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'size'        (overlay x y) == 'size' x        + 'size' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Fold a -> Fold a -> Fold a
overlay x y = Fold $ \e v o c -> runFold x e v o c `o` runFold y e v o c

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'isEmpty'     (connect x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y) >= 'vertexCount' x
-- 'vertexCount' (connect x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'edgeCount' x
-- 'edgeCount'   (connect x y) >= 'edgeCount' y
-- 'edgeCount'   (connect x y) >= 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (connect x y) <= 'vertexCount' x * 'vertexCount' y + 'edgeCount' x + 'edgeCount' y
-- 'size'        (connect x y) == 'size' x        + 'size' y
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- @
connect :: Fold a -> Fold a -> Fold a
connect x y = Fold $ \e v o c -> runFold x e v o c `c` runFold y e v o c

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
vertices :: [a] -> Fold a
vertices = overlays . map vertex

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges :: [(a, a)] -> Fold a
edges es = Fold $ \e v o c -> foldr (flip o . uncurry (c `on` v)) e es

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
overlays :: [Fold a] -> Fold a
overlays = foldr overlay empty

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: [Fold a] -> Fold a
connects = foldr connect empty

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, vertex, overlay and connect.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg 'empty' 'vertex'        'overlay' 'connect'        == id
-- foldg 'empty' 'vertex'        'overlay' (flip 'connect') == 'transpose'
-- foldg []    return        (++)    (++)           == 'Data.Foldable.toList'
-- foldg 0     (const 1)     (+)     (+)            == 'Data.Foldable.length'
-- foldg 1     (const 1)     (+)     (+)            == 'size'
-- foldg True  (const False) (&&)    (&&)           == 'isEmpty'
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Fold a -> b
foldg e v o c g = runFold g e v o c

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
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf :: Ord a => Fold a -> Fold a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: Fold a -> Bool
isEmpty = T.isEmpty

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
size :: Fold a -> Int
size = T.size

-- | Check if a graph contains a given vertex. A convenient alias for `elem`.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Eq a => a -> Fold a -> Bool
hasVertex = T.hasVertex

-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Eq a => a -> a -> Fold a -> Bool
hasEdge = T.hasEdge

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: Ord a => Fold a -> Int
vertexCount = T.vertexCount

-- | The number of edges in a graph.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => Fold a -> Int
edgeCount = T.edgeCount

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Ord a => Fold a -> [a]
vertexList = T.vertexList

-- | The sorted list of edges of a graph.
-- Complexity: /O(s + m * log(m))/ time and /O(m)/ memory. Note that the number of
-- edges /m/ of a graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- edgeList . 'transpose'    == 'Data.List.sort' . map 'Data.Tuple.swap' . edgeList
-- @
edgeList :: Ord a => Fold a -> [(a, a)]
edgeList = T.edgeList

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- vertexSet . 'clique'   == Set.'Set.fromList'
-- @
vertexSet :: Ord a => Fold a -> Set.Set a
vertexSet = T.vertexSet

-- | The set of vertices of a given graph. Like 'vertexSet' but specialised for
-- graphs with vertices of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexIntSet 'empty'      == IntSet.'IntSet.empty'
-- vertexIntSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexIntSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexIntSet :: Fold Int -> IntSet.IntSet
vertexIntSet = T.vertexIntSet

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Ord a => Fold a -> Set.Set (a, a)
edgeSet = T.edgeSet

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- adjacencyList 'empty'               == []
-- adjacencyList ('vertex' x)          == [(x, [])]
-- adjacencyList ('edge' 1 2)          == [(1, [2]), (2, [])]
-- adjacencyList ('star' 2 [3,1])      == [(1, []), (2, [1,3]), (3, [])]
-- 'fromAdjacencyList' . adjacencyList == id
-- @
adjacencyList :: Ord a => Fold a -> [(a, [a])]
adjacencyList = T.adjacencyList

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path :: [a] -> Fold a
path xs = case xs of []     -> empty
                     [x]    -> vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edges' [(x,y), (y,x)]
-- circuit . 'reverse' == 'transpose' . circuit
-- @
circuit :: [a] -> Fold a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique . 'reverse'  == 'transpose' . clique
-- @
clique :: [a] -> Fold a
clique = connects . map vertex

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: [a] -> [a] -> Fold a
biclique xs [] = vertices xs
biclique [] ys = vertices ys
biclique xs ys = connect (vertices xs) (vertices ys)

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: a -> [a] -> Fold a
star x [] = vertex x
star x ys = connect (vertex x) (vertices ys)

-- | The /star transpose/ formed by a list of leaves connected to a centre vertex.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- starTranspose x []    == 'vertex' x
-- starTranspose x [y]   == 'edge' y x
-- starTranspose x [y,z] == 'edges' [(y,x), (z,x)]
-- starTranspose x ys    == 'connect' ('vertices' ys) ('vertex' x)
-- starTranspose x ys    == 'transpose' ('star' x ys)
-- @
starTranspose :: a -> [a] -> Fold a
starTranspose x [] = vertex x
starTranspose x ys = connect (vertices ys) (vertex x)

-- | Remove a vertex from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Eq a => a -> Fold a -> Fold a
removeVertex v = induce (/= v)

-- | Remove an edge from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- 'size' (removeEdge x y z)         <= 3 * 'size' z
-- @
removeEdge :: Eq a => a -> a -> Fold a -> Fold a
removeEdge s t = filterContext s (/=s) (/=t)

-- TODO: Export
-- | Filter vertices in a subgraph context.
filterContext :: Eq a => a -> (a -> Bool) -> (a -> Bool) -> Fold a -> Fold a
filterContext s i o g = maybe g go $ G.context (==s) (toGraph g)
  where
    go (G.Context is os) = induce (/=s) g `overlay` starTranspose s (filter i is)
                                          `overlay` star          s (filter o os)

-- | Transpose a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- transpose ('box' x y)   == 'box' (transpose x) (transpose y)
-- 'edgeList' . transpose  == 'Data.List.sort' . map 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: Fold a -> Fold a
transpose = foldg empty vertex overlay (flip connect)

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- induce (const True ) x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> Fold a -> Fold a
induce p = foldg empty (\x -> if p x then vertex x else empty) (k overlay) (k connect)
  where
    k f x y | isEmpty x = y -- Constant folding to get rid of Empty leaves
            | isEmpty y = x
            | otherwise = f x y

-- | Simplify a graph expression. Semantically, this is the identity function,
-- but it simplifies a given polymorphic graph expression according to the laws
-- of the algebra. The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
-- Complexity: the function performs /O(s)/ graph comparisons. It is guaranteed
-- that the size of the result does not exceed the size of the given expression.
-- Below the operator @~>@ denotes the /is simplified to/ relation.
--
-- @
-- simplify             == id
-- 'size' (simplify x)    <= 'size' x
-- simplify 'empty'       ~> 'empty'
-- simplify 1           ~> 1
-- simplify (1 + 1)     ~> 1
-- simplify (1 + 2 + 1) ~> 1 + 2
-- simplify (1 * 1 * 1) ~> 1 * 1
-- @
simplify :: Ord a => Fold a -> Fold a
simplify = foldg empty vertex (simple overlay) (simple connect)

simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y
