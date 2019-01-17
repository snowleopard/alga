{-# LANGUAGE CPP, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.NonEmpty
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the data type 'Graph' for algebraic graphs that are known
-- to be non-empty at compile time. To avoid name clashes with "Algebra.Graph",
-- this module can be imported qualified:
--
-- @
-- import qualified Algebra.Graph.NonEmpty as NonEmpty
-- @
--
-- The naming convention generally follows that of "Data.List.NonEmpty": we use
-- suffix @1@ to indicate the functions whose interface must be changed compared
-- to "Algebra.Graph", e.g. 'vertices1'.
--
-----------------------------------------------------------------------------
module Algebra.Graph.NonEmpty (
    -- * Non-empty algebraic graphs
    Graph (..), toNonEmpty,

    -- * Basic graph construction primitives
    vertex, edge, overlay, overlay1, connect, vertices1, edges1, overlays1,
    connects1,

    -- * Graph folding
    foldg1,

    -- * Relations on graphs
    isSubgraphOf, (===),

    -- * Graph properties
    size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList1, edgeList,
    vertexSet, edgeSet,

    -- * Standard families of graphs
    path1, circuit1, clique1, biclique1, star, stars1, tree, mesh1, torus1,

    -- * Graph transformation
    removeVertex1, removeEdge, replaceVertex, mergeVertices, splitVertex1,
    transpose, induce1, simplify, sparsify,

    -- * Graph composition
    box
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq
import Control.Monad.Compat
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))

import Algebra.Graph.Internal

import qualified Algebra.Graph                 as G
import qualified Algebra.Graph.ToGraph         as T
import qualified Algebra.Graph.AdjacencyMap    as AM
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import qualified Data.IntSet                   as IntSet
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Set                      as Set
import qualified Data.Tree                     as Tree

{-| Non-empty algebraic graphs, which are constructed using three primitives:
'vertex', 'overlay' and 'connect'. See module "Algebra.Graph" for algebraic
graphs that can be empty.

We define a 'Num' instance as a convenient notation for working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

__Note:__ the 'signum' method of the type class 'Num' cannot be implemented and
will throw an error. Furthermore, the 'Num' instance does not satisfy several
"customary laws" of 'Num', which dictate that 'fromInteger' @0@ and
'fromInteger' @1@ should act as additive and multiplicative identities, and
'negate' as additive inverse. Nevertheless, overloading 'fromInteger', '+' and
'*' is very convenient when working with algebraic graphs; we hope that in
future Haskell's Prelude will provide a more fine-grained class hierarchy for
algebraic structures, which we would be able to utilise without violating any
laws.

The 'Eq' instance satisfies the following laws of non-empty algebraic graphs.

    * 'overlay' is commutative, associative and idempotent:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z
        >       x + x == x

    * 'connect' is associative:

        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

    * 'connect' satisfies absorption and saturation:

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ will
denote the number of vertices in the graph, /m/ will denote the number of
edges in the graph, and /s/ will denote the /size/ of the corresponding 'Graph'
expression, defined as the number of vertex leaves (note that /n/ <= /s/). If
@g@ is a 'Graph', the corresponding /n/, /m/ and /s/ can be computed as follows:

@n == 'vertexCount' g
m == 'edgeCount' g
s == 'size' g@

Converting a 'Graph' to the corresponding
'Algebra.Graph.NonEmpty.AdjacencyMap.AdjacencyMap' takes /O(s + m * log(m))/ time and /O(s + m)/ memory. This is also the
complexity of the graph equality test, because it is currently implemented by
converting graph expressions to canonical representations based on adjacency
maps.

The total order 'Ord' on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Here are a few examples:

@'vertex' 1 < 'vertex' 2
'vertex' 3 < 'edge' 1 2
'vertex' 1 < 'edge' 1 1
'edge' 1 1 < 'edge' 1 2
'edge' 1 2 < 'edge' 1 1 + 'edge' 2 2
'edge' 1 2 < 'edge' 1 3@

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'isSubgraphOf' x y ==> x <= y@

@x     <= x + y
x + y <= x * y@
-}
data Graph a = Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving (Functor, Show)

instance NFData a => NFData (Graph a) where
    rnf (Vertex  x  ) = rnf x
    rnf (Overlay x y) = rnf x `seq` rnf y
    rnf (Connect x y) = rnf x `seq` rnf y

instance T.ToGraph (Graph a) where
    type ToVertex (Graph a) = a
    foldg _ = foldg1
    hasEdge = hasEdge

-- | __Note:__ this does not satisfy the usual ring laws; see 'Graph' for more
-- details.
instance Num a => Num (Graph a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = error "NonEmpty.Graph.signum cannot be implemented."
    abs         = id
    negate      = id

instance Ord a => Eq (Graph a) where
    (==) = eq

instance Ord a => Ord (Graph a) where
    compare = ord

-- TODO: Find a more efficient equality check.
-- | Check if two graphs are equal by converting them to their adjacency maps.
eq :: Ord a => Graph a -> Graph a -> Bool
eq x y = T.toAdjacencyMap x == T.toAdjacencyMap y
{-# NOINLINE [1] eq #-}
{-# RULES "eqInt" eq = eqInt #-}

-- Like @eq@ but specialised for graphs with vertices of type 'Int'.
eqInt :: Graph Int -> Graph Int -> Bool
eqInt x y = T.toAdjacencyIntMap x == T.toAdjacencyIntMap y

-- TODO: Find a more efficient comparison.
-- Compare two graphs by converting them to their adjacency maps.
ord :: Ord a => Graph a -> Graph a -> Ordering
ord x y = compare (T.toAdjacencyMap x) (T.toAdjacencyMap y)
{-# NOINLINE [1] ord #-}
{-# RULES "ordInt" ord = ordInt #-}

-- Like @ord@ but specialised for graphs with vertices of type 'Int'.
ordInt :: Graph Int -> Graph Int -> Ordering
ordInt x y = compare (T.toAdjacencyIntMap x) (T.toAdjacencyIntMap y)

instance Applicative Graph where
    pure    = Vertex
    f <*> x = f >>= (<$> x)

instance Monad Graph where
    return  = pure
    g >>= f = foldg1 f Overlay Connect g

-- | Convert an algebraic graph (from "Algebra.Graph") into a non-empty
-- algebraic graph. Returns 'Nothing' if the argument is 'G.empty'.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- toNonEmpty 'G.empty'       == Nothing
-- toNonEmpty ('T.toGraph' x) == Just (x :: 'Graph' a)
-- @
toNonEmpty :: G.Graph a -> Maybe (Graph a)
toNonEmpty = G.foldg Nothing (Just . Vertex) (go Overlay) (go Connect)
  where
    go _ Nothing  y        = y
    go _ x        Nothing  = x
    go f (Just x) (Just y) = Just (f x y)

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'hasVertex' x (vertex x) == True
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- 'size'        (vertex x) == 1
-- @
vertex :: a -> Graph a
vertex = Vertex
{-# INLINE vertex #-}

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
edge :: a -> a -> Graph a
edge u v = connect (vertex u) (vertex v)

-- | /Overlay/ two graphs. An alias for the constructor 'Overlay'. This is a
-- commutative, associative and idempotent operation.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'size'        (overlay x y) == 'size' x        + 'size' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Graph a -> Graph a -> Graph a
overlay = Overlay
{-# INLINE overlay #-}

-- | Overlay a possibly empty graph (from "Algebra.Graph") with a non-empty
-- graph. If the first argument is 'G.empty', the function returns the second
-- argument; otherwise it is semantically the same as 'overlay'.
-- Complexity: /O(s1)/ time and memory, and /O(s1 + s2)/ size.
--
-- @
--                overlay1 'G.empty' x == x
-- x /= 'G.empty' ==> overlay1 x     y == overlay (fromJust $ toNonEmpty x) y
-- @
overlay1 :: G.Graph a -> Graph a -> Graph a
overlay1 = maybe id overlay . toNonEmpty

-- | /Connect/ two graphs. An alias for the constructor 'Connect'. This is an
-- associative operation, which distributes over 'overlay' and obeys the
-- decomposition axiom.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
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
connect :: Graph a -> Graph a -> Graph a
connect = Connect
{-# INLINE connect #-}

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices1 [x]           == 'vertex' x
-- 'hasVertex' x . vertices1 == 'elem' x
-- 'vertexCount' . vertices1 == 'length' . 'Data.List.NonEmpty.nub'
-- 'vertexSet'   . vertices1 == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertices1 :: NonEmpty a -> Graph a
vertices1 = overlays1 . fmap vertex
{-# NOINLINE [1] vertices1 #-}

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges1 [(x,y)]     == 'edge' x y
-- 'edgeCount' . edges1 == 'Data.List.NonEmpty.length' . 'Data.List.NonEmpty.nub'
-- @
edges1 :: NonEmpty (a, a) -> Graph a
edges1  = overlays1 . fmap (uncurry edge)

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays1 [x]   == x
-- overlays1 [x,y] == 'overlay' x y
-- @
overlays1 :: NonEmpty (Graph a) -> Graph a
overlays1 = concatg1 overlay
{-# INLINE [2] overlays1 #-}

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- connects1 [x]   == x
-- connects1 [x,y] == 'connect' x y
-- @
connects1 :: NonEmpty (Graph a) -> Graph a
connects1 = concatg1 connect
{-# INLINE [2] connects1 #-}

-- Auxiliary function, similar to 'sconcat'.
concatg1 :: (Graph a -> Graph a -> Graph a) -> NonEmpty (Graph a) -> Graph a
concatg1 combine (x :| xs) = maybe x (combine x) $ foldr1Safe combine xs

-- | Generalised graph folding: recursively collapse a 'Graph' by
-- applying the provided functions to the leaves and internal nodes of the
-- expression. The order of arguments is: vertex, overlay and connect.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg1 'vertex'    'overlay' 'connect'        == id
-- foldg1 'vertex'    'overlay' ('flip' 'connect') == 'transpose'
-- foldg1 ('const' 1) (+)     (+)            == 'size'
-- foldg1 (== x)    (||)    (||)           == 'hasVertex' x
-- @
foldg1 :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg1 v o c = go
  where
    go (Vertex  x  ) = v x
    go (Overlay x y) = o (go x) (go y)
    go (Connect x y) = c (go x) (go y)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- isSubgraphOf x             ('overlay' x y) ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf ('path1' xs)    ('circuit1' xs) ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: Ord a => Graph a -> Graph a -> Bool
isSubgraphOf x y = AM.isSubgraphOf (T.toAdjacencyMap x) (T.toAdjacencyMap y)
{-# NOINLINE [1] isSubgraphOf #-}
{-# RULES "isSubgraphOf/Int" isSubgraphOf = isSubgraphOfIntR #-}

-- Like 'isSubgraphOf' but specialised for graphs with vertices of type 'Int'.
isSubgraphOfIntR :: Graph Int -> Graph Int -> Bool
isSubgraphOfIntR x y = AIM.isSubgraphOf (T.toAdjacencyIntMap x) (T.toAdjacencyIntMap y)

-- | Structural equality on graph expressions.
-- Complexity: /O(s)/ time.
--
-- @
--     x === x     == True
-- x + y === x + y == True
-- 1 + 2 === 2 + 1 == False
-- x + y === x * y == False
-- @
(===) :: Eq a => Graph a -> Graph a -> Bool
(Vertex  x1   ) === (Vertex  x2   ) = x1 ==  x2
(Overlay x1 y1) === (Overlay x2 y2) = x1 === x2 && y1 === y2
(Connect x1 y1) === (Connect x2 y2) = x1 === x2 && y1 === y2
_               === _               = False
{-# SPECIALISE (===) :: Graph Int -> Graph Int -> Bool #-}

infix 4 ===

-- | The /size/ of a graph, i.e. the number of leaves of the expression.
-- Complexity: /O(s)/ time.
--
-- @
-- size ('vertex' x)    == 1
-- size ('overlay' x y) == size x + size y
-- size ('connect' x y) == size x + size y
-- size x             >= 1
-- size x             >= 'vertexCount' x
-- @
size :: Graph a -> Int
size = foldg1 (const 1) (+) (+)

-- | Check if a graph contains a given vertex.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x ('vertex' x) == True
-- hasVertex 1 ('vertex' 2) == False
-- @
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex v = foldg1 (==v) (||) (||)
{-# SPECIALISE hasVertex :: Int -> Graph Int -> Bool #-}

-- TODO: Reduce code duplication with 'Algebra.Graph.hasEdge'.
-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Eq a => a -> a -> Graph a -> Bool
hasEdge s t g = hit g == Edge
  where
    hit (Vertex x   ) = if x == s then Tail else Miss
    hit (Overlay x y) = case hit x of
        Miss -> hit y
        Tail -> max Tail (hit y)
        Edge -> Edge
    hit (Connect x y) = case hit x of
        Miss -> hit y
        Tail -> if hasVertex t y then Edge else Tail
        Edge -> Edge
{-# SPECIALISE hasEdge :: Int -> Int -> Graph Int -> Bool #-}

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: Ord a => Graph a -> Int
vertexCount = T.vertexCount
{-# RULES "vertexCount/Int" vertexCount = vertexIntCount #-}
{-# INLINE [1] vertexCount #-}

-- Like 'vertexCount' but specialised for Graph with vertices of type 'Int'.
vertexIntCount :: Graph Int -> Int
vertexIntCount = IntSet.size . vertexIntSet

-- | The number of edges in a graph.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => Graph a -> Int
edgeCount = T.edgeCount
{-# INLINE [1] edgeCount #-}
{-# RULES "edgeCount/Int" edgeCount = edgeCountInt #-}

-- Like 'edgeCount' but specialised for graphs with vertices of type 'Int'.
edgeCountInt :: Graph Int -> Int
edgeCountInt = T.edgeCount . T.toAdjacencyIntMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList1 ('vertex' x)  == [x]
-- vertexList1 . 'vertices1' == 'Data.List.NonEmpty.nub' . 'Data.List.NonEmpty.sort'
-- @
vertexList1 :: Ord a => Graph a -> NonEmpty a
vertexList1 = NonEmpty.fromList . Set.toAscList . vertexSet
{-# RULES "vertexList1/Int" vertexList1 = vertexIntList1 #-}
{-# INLINE [1] vertexList1 #-}

-- | Like 'vertexList1' but specialised for Graph with vertices of type 'Int'.
vertexIntList1 :: Graph Int -> NonEmpty Int
vertexIntList1 = NonEmpty.fromList . IntSet.toAscList . vertexIntSet

-- | The sorted list of edges of a graph.
-- Complexity: /O(s + m * log(m))/ time and /O(m)/ memory. Note that the number of
-- edges /m/ of a graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges1'       == 'Data.List.nub' . 'Data.List.sort' . 'Data.List.NonEmpty.toList'
-- edgeList . 'transpose'    == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . edgeList
-- @
edgeList :: Ord a => Graph a -> [(a, a)]
edgeList = T.edgeList
{-# RULES "edgeList/Int" edgeList = edgeIntList #-}
{-# INLINE [1] edgeList #-}

-- Like 'edgeList' but specialised for Graph with vertices of type 'Int'.
edgeIntList :: Graph Int -> [(Int, Int)]
edgeIntList = T.edgeList . T.toAdjacencyIntMap

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet . 'vertex'    == Set.'Set.singleton'
-- vertexSet . 'vertices1' == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- vertexSet . 'clique1'   == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertexSet :: Ord a => Graph a -> Set.Set a
vertexSet = T.vertexSet

-- Like 'vertexSet' but specialised for graphs with vertices of type 'Int'.
vertexIntSet :: Graph Int -> IntSet.IntSet
vertexIntSet = T.vertexIntSet

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges1'   == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
edgeSet :: Ord a => Graph a -> Set.Set (a, a)
edgeSet = T.edgeSet

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path1 [x]       == 'vertex' x
-- path1 [x,y]     == 'edge' x y
-- path1 . 'Data.List.NonEmpty.reverse' == 'transpose' . path1
-- @
path1 :: NonEmpty a -> Graph a
path1 (x :| []    ) = vertex x
path1 (x :| (y:ys)) = edges1 ((x, y) :| zip (y:ys) ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit1 [x]       == 'edge' x x
-- circuit1 [x,y]     == 'edges1' [(x,y), (y,x)]
-- circuit1 . 'Data.List.NonEmpty.reverse' == 'transpose' . circuit1
-- @
circuit1 :: NonEmpty a -> Graph a
circuit1 (x :| xs) = path1 (x :| xs ++ [x])

-- | The /clique/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique1 [x]        == 'vertex' x
-- clique1 [x,y]      == 'edge' x y
-- clique1 [x,y,z]    == 'edges1' [(x,y), (x,z), (y,z)]
-- clique1 (xs '<>' ys) == 'connect' (clique1 xs) (clique1 ys)
-- clique1 . 'Data.List.NonEmpty.reverse'  == 'transpose' . clique1
-- @
clique1 :: NonEmpty a -> Graph a
clique1 = connects1 . fmap vertex
{-# NOINLINE [1] clique1 #-}

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique1 [x1,x2] [y1,y2] == 'edges1' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique1 xs      ys      == 'connect' ('vertices1' xs) ('vertices1' ys)
-- @
biclique1 :: NonEmpty a -> NonEmpty a -> Graph a
biclique1 xs ys = connect (vertices1 xs) (vertices1 ys)

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges1' [(x,y), (x,z)]
-- @
star :: a -> [a] -> Graph a
star x []     = vertex x
star x (y:ys) = connect (vertex x) (vertices1 $ y :| ys)
{-# INLINE star #-}

-- | The /stars/ formed by overlaying a non-empty list of 'star's.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the total size of the
-- input.
--
-- @
-- stars1 [(x, [] )]               == 'vertex' x
-- stars1 [(x, [y])]               == 'edge' x y
-- stars1 [(x, ys )]               == 'star' x ys
-- stars1                          == 'overlays1' . 'fmap' ('uncurry' 'star')
-- 'overlay' (stars1 xs) (stars1 ys) == stars1 (xs '<>' ys)
-- @
stars1 :: NonEmpty (a, [a]) -> Graph a
stars1 = overlays1 . fmap (uncurry star)
{-# INLINE stars1 #-}

-- | The /tree graph/ constructed from a given 'Tree.Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree (i.e. the number of vertices in the tree).
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path1' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges1' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Tree.Tree a -> Graph a
tree (Tree.Node x f) = overlays1 $ star x (map Tree.rootLabel f) :| map tree f

-- | Construct a /mesh graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh1 [x]     [y]        == 'vertex' (x, y)
-- mesh1 xs      ys         == 'box' ('path1' xs) ('path1' ys)
-- mesh1 [1,2,3] [\'a\', \'b\'] == 'edges1' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\'))
--                                    , ((1,\'b\'),(2,\'b\')), ((2,\'a\'),(2,\'b\'))
--                                    , ((2,\'a\'),(3,\'a\')), ((2,\'b\'),(3,\'b\'))
--                                    , ((3,\'a\'),(3,\'b\')) ]
-- @
mesh1 :: NonEmpty a -> NonEmpty b -> Graph (a, b)
mesh1 xx@(x:|xs) yy@(y:|ys) =
  case NonEmpty.nonEmpty ipxs of
    Nothing ->
      case NonEmpty.nonEmpty ipys of
        Nothing    -> vertex (x,y)
        Just ipys' ->
          stars1 $ fmap (\(y1,y2) -> ((x,y1), [(x,y2)]) ) ipys'
    Just ipxs' ->
      case NonEmpty.nonEmpty ipys of
        Nothing ->
          stars1 $ fmap (\(x1,x2) -> ((x1,y), [(x2,y)]) ) ipxs'
        Just ipys' ->
          stars1 $
            appendNonEmpty (fmap (\((a1,a2),(b1,b2)) -> ((a1, b1), [(a1, b2), (a2, b1)])) $ liftM2 (,) ipxs' ipys') $
              [ ((lx,y1), [(lx,y2)]) | (y1,y2) <- ipys]
           ++ [ ((x1,ly), [(x2,ly)]) | (x1,x2) <- ipxs]
  where
    lx = last xs
    ly = last ys
    ipxs = NonEmpty.init (pairs1 xx)
    ipys = NonEmpty.init (pairs1 yy)

-- | Construct a /torus graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus1 [x]   [y]        == 'edge' (x,y) (x,y)
-- torus1 xs    ys         == 'box' ('circuit1' xs) ('circuit1' ys)
-- torus1 [1,2] [\'a\', \'b\'] == 'edges1' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\'))
--                                   , ((1,\'b\'),(1,\'a\')), ((1,\'b\'),(2,\'b\'))
--                                   , ((2,\'a\'),(1,\'a\')), ((2,\'a\'),(2,\'b\'))
--                                   , ((2,\'b\'),(1,\'b\')), ((2,\'b\'),(2,\'a\')) ]
-- @
torus1 :: NonEmpty a -> NonEmpty b -> Graph (a, b)
torus1 xs ys = stars1 $ fmap (\((a1,a2),(b1,b2)) -> ((a1, b1), [(a1, b2), (a2, b1)]))
    $ liftM2 (,) (pairs1 xs) (pairs1 ys)

-- Auxiliary function for 'mesh1' and 'torus1'
pairs1 :: NonEmpty a -> NonEmpty (a, a)
pairs1 as@(x:|xs) = NonEmpty.zip as $ maybe (x :| []) (`appendNonEmpty` [x]) $ NonEmpty.nonEmpty xs

-- Append a list to a non-empty one
appendNonEmpty :: NonEmpty a -> [a] -> NonEmpty a
appendNonEmpty (w:|ws) zs = w :| (ws++zs)

-- | Remove a vertex from a given graph. Returns @Nothing@ if the resulting
-- graph is empty.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeVertex1 x ('vertex' x)          == Nothing
-- removeVertex1 1 ('vertex' 2)          == Just ('vertex' 2)
-- removeVertex1 x ('edge' x x)          == Nothing
-- removeVertex1 1 ('edge' 1 2)          == Just ('vertex' 2)
-- removeVertex1 x '>=>' removeVertex1 x == removeVertex1 x
-- @
removeVertex1 :: Eq a => a -> Graph a -> Maybe (Graph a)
removeVertex1 x = induce1 (/= x)
{-# SPECIALISE removeVertex1 :: Int -> Graph Int -> Maybe (Graph Int) #-}

-- | Remove an edge from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices1' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- 'size' (removeEdge x y z)         <= 3 * 'size' z
-- @
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge s t = filterContext s (/=s) (/=t)
{-# SPECIALISE removeEdge :: Int -> Int -> Graph Int -> Graph Int #-}

-- TODO: Export
filterContext :: Eq a => a -> (a -> Bool) -> (a -> Bool) -> Graph a -> Graph a
filterContext s i o g = maybe g go $ G.context (==s) (T.toGraph g)
  where
    go (G.Context is os) = G.induce (/=s) (T.toGraph g)     `overlay1`
                           transpose (star s (filter i is)) `overlay` star s (filter o os)
{-# SPECIALISE filterContext :: Int -> (Int -> Bool) -> (Int -> Bool) -> Graph Int -> Graph Int #-}

-- | The function 'replaceVertex' @x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Eq a => a -> a -> Graph a -> Graph a
replaceVertex u v = fmap $ \w -> if w == u then v else w
{-# SPECIALISE replaceVertex :: Int -> Int -> Graph Int -> Graph Int #-}

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> Graph a -> Graph a
mergeVertices p v = fmap $ \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
-- Complexity: /O(s + k * L)/ time, memory and size, where /k/ is the number of
-- occurrences of the vertex in the expression and /L/ is the length of the
-- given list.
--
-- @
-- splitVertex1 x [x]                 == id
-- splitVertex1 x [y]                 == 'replaceVertex' x y
-- splitVertex1 1 [0,1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex1 :: Eq a => a -> NonEmpty a -> Graph a -> Graph a
splitVertex1 v us g = g >>= \w -> if w == v then vertices1 us else vertex w
{-# SPECIALISE splitVertex1 :: Int -> NonEmpty Int -> Graph Int -> Graph Int #-}

-- | Transpose a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- transpose ('box' x y)   == 'box' (transpose x) (transpose y)
-- 'edgeList' . transpose  == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: Graph a -> Graph a
transpose = foldg1 vertex overlay (flip connect)
{-# NOINLINE [1] transpose #-}

{-# RULES
"transpose/Vertex"   forall x. transpose (Vertex x) = Vertex x
"transpose/Overlay"  forall g1 g2. transpose (Overlay g1 g2) = Overlay (transpose g1) (transpose g2)
"transpose/Connect"  forall g1 g2. transpose (Connect g1 g2) = Connect (transpose g2) (transpose g1)

"transpose/overlays1" forall xs. transpose (overlays1 xs) = overlays1 (fmap transpose xs)
"transpose/connects1" forall xs. transpose (connects1 xs) = connects1 (NonEmpty.reverse (fmap transpose xs))

"transpose/vertices1" forall xs. transpose (vertices1 xs) = vertices1 xs
"transpose/clique1"   forall xs. transpose (clique1 xs) = clique1 (NonEmpty.reverse xs)
 #-}

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate. Returns @Nothing@ if the
-- resulting graph is empty.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- induce1 ('const' True ) x == Just x
-- induce1 ('const' False) x == Nothing
-- induce1 (/= x)          == 'removeVertex1' x
-- induce1 p '>=>' induce1 q == induce1 (\\x -> p x && q x)
-- @
induce1 :: (a -> Bool) -> Graph a -> Maybe (Graph a)
induce1 p = foldg1
  (\x -> if p x then Just (Vertex x) else Nothing)
  (k Overlay)
  (k Connect)
  where
    k _ Nothing a = a
    k _ a Nothing = a
    k f (Just a) (Just b) = Just $ f a b

-- | Simplify a graph expression. Semantically, this is the identity function,
-- but it simplifies a given expression according to the laws of the algebra.
-- The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
-- Complexity: the function performs /O(s)/ graph comparisons. It is guaranteed
-- that the size of the result does not exceed the size of the given expression.
--
-- @
-- simplify             ==  id
-- 'size' (simplify x)    <=  'size' x
-- simplify 1           '===' 1
-- simplify (1 + 1)     '===' 1
-- simplify (1 + 2 + 1) '===' 1 + 2
-- simplify (1 * 1 * 1) '===' 1 * 1
-- @
simplify :: Ord a => Graph a -> Graph a
simplify = foldg1 Vertex (simple Overlay) (simple Connect)
{-# SPECIALISE simplify :: Graph Int -> Graph Int #-}

simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y
{-# SPECIALISE simple :: (Graph Int -> Graph Int -> Graph Int) -> Graph Int -> Graph Int -> Graph Int #-}

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O(s1 * s2)/ time, memory and size, where /s1/ and /s2/ are the
-- sizes of the given graphs.
--
-- @
-- box ('path1' [0,1]) ('path1' [\'a\',\'b\']) == 'edges1' [ ((0,\'a\'), (0,\'b\'))
--                                               , ((0,\'a\'), (1,\'a\'))
--                                               , ((0,\'b\'), (1,\'b\'))
--                                               , ((1,\'a\'), (1,\'b\')) ]
-- @
-- Up to an isomorphism between the resulting vertex types, this operation
-- is /commutative/, /associative/, /distributes/ over 'overlay', and has
-- singleton graphs as /identities/. Below @~~@ stands for the equality up to an
-- isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y               ~~ box y x
-- box x (box y z)       ~~ box (box x y) z
-- box x ('overlay' y z)   == 'overlay' (box x y) (box x z)
-- box x ('vertex' ())     ~~ x
-- 'transpose'   (box x y) == box ('transpose' x) ('transpose' y)
-- 'vertexCount' (box x y) == 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (box x y) <= 'vertexCount' x * 'edgeCount' y + 'edgeCount' x * 'vertexCount' y
-- @
box :: Graph a -> Graph b -> Graph (a, b)
box x y = overlay (fx <*> y) (fy <*> x)
  where
    fx = foldg1 (vertex .      (,)) overlay overlay x
    fy = foldg1 (vertex . flip (,)) overlay overlay y

-- | /Sparsify/ a graph by adding intermediate 'Left' @Int@ vertices between the
-- original vertices (wrapping the latter in 'Right') such that the resulting
-- graph is /sparse/, i.e. contains only O(s) edges, but preserves the
-- reachability relation between the original vertices. Sparsification is useful
-- when working with dense graphs, as it can reduce the number of edges from
-- O(n^2) down to O(n) by replacing cliques, bicliques and similar densely
-- connected structures by sparse subgraphs built out of intermediate vertices.
-- Complexity: O(s) time, memory and size.
--
-- @
-- 'Data.List.sort' . 'Algebra.Graph.ToGraph.reachable' x       == 'Data.List.sort' . 'Data.Either.rights' . 'Algebra.Graph.ToGraph.reachable' ('Data.Either.Right' x) . sparsify
-- 'vertexCount' (sparsify x) <= 'vertexCount' x + 'size' x + 1
-- 'edgeCount'   (sparsify x) <= 3 * 'size' x
-- 'size'        (sparsify x) <= 3 * 'size' x
-- @
sparsify :: Graph a -> Graph (Either Int a)
sparsify graph = res
  where
    (res, end) = runState (foldg1 v o c graph 0 end) 1
    v x   s t  = return $ clique1 (Left s :| [Right x, Left t])
    o x y s t  = overlay <$> s `x` t <*> s `y` t
    c x y s t  = do
        m <- get
        put (m + 1)
        overlay <$> s `x` m <*> m `y` t
