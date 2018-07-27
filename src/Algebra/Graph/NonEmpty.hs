{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
-- This module defines the data type 'NonEmptyGraph' for graphs that are known
-- to be non-empty at compile time. The naming convention generally follows that
-- of "Data.List.NonEmpty": we use suffix @1@ to indicate the functions whose
-- interface must be changed compared to "Algebra.Graph", e.g. 'vertices1'.
--
-----------------------------------------------------------------------------
module Algebra.Graph.NonEmpty (
    -- * Algebraic data type for non-empty graphs
    NonEmptyGraph (..), toNonEmptyGraph,

    -- * Basic graph construction primitives
    vertex, edge, overlay, overlay1, connect, vertices1, edges1, overlays1,
    connects1,

    -- * Graph folding
    foldg1,

    -- * Relations on graphs
    isSubgraphOf, (===),

    -- * Graph properties
    size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList1, edgeList,
    vertexSet, vertexIntSet, edgeSet,

    -- * Standard families of graphs
    path1, circuit1, clique1, biclique1, star, starTranspose, tree, mesh1, torus1,

    -- * Graph transformation
    removeVertex1, removeEdge, replaceVertex, mergeVertices, splitVertex1,
    transpose, induce1, simplify,

    -- * Graph composition
    box
  ) where

import Prelude ()
import Prelude.Compat

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import Control.DeepSeq (NFData (..))
import Control.Monad.Compat
import Data.List.NonEmpty (NonEmpty (..))

import Algebra.Graph.Internal

import qualified Algebra.Graph                 as G
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import qualified Algebra.Graph.ToGraph         as T
import qualified Data.IntSet                   as IntSet
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Set                      as Set
import qualified Data.Tree                     as Tree

{-| The 'NonEmptyGraph' data type is a deep embedding of the core graph
construction primitives 'vertex', 'overlay' and 'connect'. As one can guess from
the name, the empty graph cannot be represented using this data type. See module
"Algebra.Graph" for a graph data type that allows for the construction of the
empty graph.

We define a 'Num' instance as a convenient notation for working with graphs:

    > 0           == Vertex 0
    > 1 + 2       == Overlay (Vertex 1) (Vertex 2)
    > 1 * 2       == Connect (Vertex 1) (Vertex 2)
    > 1 + 2 * 3   == Overlay (Vertex 1) (Connect (Vertex 2) (Vertex 3))
    > 1 * (2 + 3) == Connect (Vertex 1) (Overlay (Vertex 2) (Vertex 3))

Note that the 'signum' method of the 'Num' type class cannot be implemented.

The 'Eq' instance is currently implemented using the 'AM.AdjacencyMap' as the
/canonical graph representation/ and satisfies the following laws of algebraic
graphs:

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
edges in the graph, and /s/ will denote the /size/ of the corresponding
'NonEmptyGraph' expression, defined as the number of vertex leaves. For example,
if @g@ is a 'NonEmptyGraph' then /n/, /m/ and /s/ can be computed as follows:

@n == 'vertexCount' g
m == 'edgeCount' g
s == 'size' g@

The 'size' of any graph is positive and coincides with the result of 'length'
method of the 'Foldable' type class. We define 'size' only for the consistency
with the API of other graph representations, such as "Algebra.Graph".

Converting a 'NonEmptyGraph' to the corresponding 'AM.AdjacencyMap' takes
/O(s + m * log(m))/ time and /O(s + m)/ memory. This is also the complexity of
the graph equality test, because it is currently implemented by converting graph
expressions to canonical representations based on adjacency maps.
-}
data NonEmptyGraph a = Vertex a
                     | Overlay (NonEmptyGraph a) (NonEmptyGraph a)
                     | Connect (NonEmptyGraph a) (NonEmptyGraph a)
                     deriving (Foldable, Functor, Show, Traversable)

instance NFData a => NFData (NonEmptyGraph a) where
    rnf (Vertex  x  ) = rnf x
    rnf (Overlay x y) = rnf x `seq` rnf y
    rnf (Connect x y) = rnf x `seq` rnf y

instance T.ToGraph (NonEmptyGraph a) where
    type ToVertex (NonEmptyGraph a) = a
    foldg _ = foldg1
    hasEdge = hasEdge

instance Num a => Num (NonEmptyGraph a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = error "NonEmptyGraph.signum cannot be implemented."
    abs         = id
    negate      = id

instance Ord a => Eq (NonEmptyGraph a) where
    (==) = equals

-- TODO: Find a more efficient equality check.
-- | Compare two graphs by converting them to their adjacency maps.
{-# NOINLINE [1] equals #-}
{-# RULES "equalsInt" equals = equalsInt #-}
equals :: Ord a => NonEmptyGraph a -> NonEmptyGraph a -> Bool
equals x y = T.adjacencyMap x == T.adjacencyMap y

-- | Like 'equals' but specialised for graphs with vertices of type 'Int'.
equalsInt :: NonEmptyGraph Int -> NonEmptyGraph Int -> Bool
equalsInt x y = T.adjacencyIntMap x == T.adjacencyIntMap y

instance Applicative NonEmptyGraph where
    pure  = Vertex
    (<*>) = ap

instance Monad NonEmptyGraph where
    return  = pure
    g >>= f = foldg1 f Overlay Connect g

-- | Convert a 'G.Graph' into 'NonEmptyGraph'. Returns 'Nothing' if the argument
-- is 'G.empty'.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- toNonEmptyGraph 'G.empty'       == Nothing
-- toNonEmptyGraph ('C.toGraph' x) == Just (x :: NonEmptyGraph a)
-- @
toNonEmptyGraph :: G.Graph a -> Maybe (NonEmptyGraph a)
toNonEmptyGraph = G.foldg Nothing (Just . Vertex) (go Overlay) (go Connect)
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
vertex :: a -> NonEmptyGraph a
vertex = Vertex

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
edge :: a -> a -> NonEmptyGraph a
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
overlay :: NonEmptyGraph a -> NonEmptyGraph a -> NonEmptyGraph a
overlay = Overlay

-- | Overlay a possibly empty graph with a non-empty graph. If the first
-- argument is 'G.empty', the function returns the second argument; otherwise
-- it is semantically the same as 'overlay'.
-- Complexity: /O(s1)/ time and memory, and /O(s1 + s2)/ size.
--
-- @
--                overlay1 'G.empty' x == x
-- x /= 'G.empty' ==> overlay1 x     y == overlay (fromJust $ toNonEmptyGraph x) y
-- @
overlay1 :: G.Graph a -> NonEmptyGraph a -> NonEmptyGraph a
overlay1 = maybe id overlay . toNonEmptyGraph

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
connect :: NonEmptyGraph a -> NonEmptyGraph a -> NonEmptyGraph a
connect = Connect

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices1 (x ':|' [])     == 'vertex' x
-- 'hasVertex' x . vertices1 == 'elem' x
-- 'vertexCount' . vertices1 == 'length' . 'Data.List.NonEmpty.nub'
-- 'vertexSet'   . vertices1 == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertices1 :: NonEmpty a -> NonEmptyGraph a
vertices1 (x :| xs) = foldr (Overlay . vertex) (vertex x) xs

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges1 ((x,y) ':|' []) == 'edge' x y
-- 'edgeCount' . edges1   == 'Data.List.NonEmpty.length' . 'Data.List.NonEmpty.nub'
-- @
edges1 :: NonEmpty (a, a) -> NonEmptyGraph a
edges1 (x :| xs) = foldr (Overlay . uncurry edge) (uncurry edge x) xs

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays1 (x ':|' [] ) == x
-- overlays1 (x ':|' [y]) == 'overlay' x y
-- @
overlays1 :: NonEmpty (NonEmptyGraph a) -> NonEmptyGraph a
overlays1 = foldr1 overlay

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- connects1 (x ':|' [] ) == x
-- connects1 (x ':|' [y]) == 'connect' x y
-- @
connects1 :: NonEmpty (NonEmptyGraph a) -> NonEmptyGraph a
connects1 = foldr1 connect

-- | Generalised graph folding: recursively collapse a 'NonEmptyGraph' by
-- applying the provided functions to the leaves and internal nodes of the
-- expression. The order of arguments is: vertex, overlay and connect.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg1 (const 1) (+)  (+)  == 'size'
-- foldg1 (==x)     (||) (||) == 'hasVertex' x
-- @
foldg1 :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> NonEmptyGraph a -> b
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
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path1' xs)    ('circuit1' xs) == True
-- @
{-# SPECIALISE isSubgraphOf :: NonEmptyGraph Int -> NonEmptyGraph Int -> Bool #-}
isSubgraphOf :: Ord a => NonEmptyGraph a -> NonEmptyGraph a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Structural equality on graph expressions.
-- Complexity: /O(s)/ time.
--
-- @
--     x === x     == True
-- x + y === x + y == True
-- 1 + 2 === 2 + 1 == False
-- x + y === x * y == False
-- @
{-# SPECIALISE (===) :: NonEmptyGraph Int -> NonEmptyGraph Int -> Bool #-}
(===) :: Eq a => NonEmptyGraph a -> NonEmptyGraph a -> Bool
(Vertex  x1   ) === (Vertex  x2   ) = x1 ==  x2
(Overlay x1 y1) === (Overlay x2 y2) = x1 === x2 && y1 === y2
(Connect x1 y1) === (Connect x2 y2) = x1 === x2 && y1 === y2
_               === _               = False

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
size :: NonEmptyGraph a -> Int
size = foldg1 (const 1) (+) (+)

-- | Check if a graph contains a given vertex. A convenient alias for `elem`.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x ('vertex' x) == True
-- hasVertex 1 ('vertex' 2) == False
-- @
{-# SPECIALISE hasVertex :: Int -> NonEmptyGraph Int -> Bool #-}
hasVertex :: Eq a => a -> NonEmptyGraph a -> Bool
hasVertex v = foldg1 (==v) (||) (||)

-- TODO: Reduce code duplication with 'Algebra.Graph.hasEdge'.
-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
{-# SPECIALISE hasEdge :: Int -> Int -> NonEmptyGraph Int -> Bool #-}
hasEdge :: Eq a => a -> a -> NonEmptyGraph a -> Bool
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

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- vertexCount ('vertex' x) == 1
-- vertexCount x          >= 1
-- vertexCount            == 'length' . 'vertexList1'
-- @
{-# RULES "vertexCount/Int" vertexCount = vertexIntCount #-}
{-# INLINE[1] vertexCount #-}
vertexCount :: Ord a => NonEmptyGraph a -> Int
vertexCount = T.vertexCount

-- | Like 'vertexCount' but specialised for NonEmptyGraph with vertices of type 'Int'.
vertexIntCount :: NonEmptyGraph Int -> Int
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
{-# SPECIALISE edgeCount :: NonEmptyGraph Int -> Int #-}
edgeCount :: Ord a => NonEmptyGraph a -> Int
edgeCount = length . edgeList

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList1 ('vertex' x)  == x ':|' []
-- vertexList1 . 'vertices1' == 'Data.List.NonEmpty.nub' . 'Data.List.NonEmpty.sort'
-- @
{-# RULES "vertexList1/Int" vertexList1 = vertexIntList1 #-}
{-# INLINE[1] vertexList1 #-}
vertexList1 :: Ord a => NonEmptyGraph a -> NonEmpty a
vertexList1 = NonEmpty.fromList . Set.toAscList . vertexSet

-- | Like 'vertexList1' but specialised for NonEmptyGraph with vertices of type 'Int'.
vertexIntList1 :: NonEmptyGraph Int -> NonEmpty Int
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
-- edgeList . 'transpose'    == 'Data.List.sort' . map 'Data.Tuple.swap' . edgeList
-- @
{-# RULES "edgeList/Int" edgeList = edgeIntList #-}
{-# INLINE[1] edgeList #-}
edgeList :: Ord a => NonEmptyGraph a -> [(a, a)]
edgeList = T.edgeList

-- | Like 'edgeList' but specialised for NonEmptyGraph with vertices of type 'Int'.
edgeIntList :: NonEmptyGraph Int -> [(Int,Int)]
edgeIntList = AIM.edgeList . foldg1 AIM.vertex AIM.overlay AIM.connect

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet . 'vertex'    == Set.'Set.singleton'
-- vertexSet . 'vertices1' == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- vertexSet . 'clique1'   == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertexSet :: Ord a => NonEmptyGraph a -> Set.Set a
vertexSet = T.vertexSet

-- | The set of vertices of a given graph. Like 'vertexSet' but specialised for
-- graphs with vertices of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexIntSet . 'vertex'    == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices1' == IntSet.'IntSet.fromList' . 'Data.List.NonEmpty.toList'
-- vertexIntSet . 'clique1'   == IntSet.'IntSet.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertexIntSet :: NonEmptyGraph Int -> IntSet.IntSet
vertexIntSet = T.vertexIntSet

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges1'   == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
edgeSet :: Ord a => NonEmptyGraph a -> Set.Set (a, a)
edgeSet = T.edgeSet

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path1 (x ':|' [] ) == 'vertex' x
-- path1 (x ':|' [y]) == 'edge' x y
-- path1 . 'Data.List.NonEmpty.reverse'  == 'transpose' . path1
-- @
path1 :: NonEmpty a -> NonEmptyGraph a
path1 (x :| []    ) = vertex x
path1 (x :| (y:ys)) = edges1 ((x, y) :| zip (y:ys) ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit1 (x ':|' [] ) == 'edge' x x
-- circuit1 (x ':|' [y]) == 'edges1' ((x,y) ':|' [(y,x)])
-- circuit1 . 'Data.List.NonEmpty.reverse'  == 'transpose' . circuit1
-- @
circuit1 :: NonEmpty a -> NonEmptyGraph a
circuit1 (x :| xs) = path1 (x :| xs ++ [x])

-- | The /clique/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique1 (x ':|' []   ) == 'vertex' x
-- clique1 (x ':|' [y]  ) == 'edge' x y
-- clique1 (x ':|' [y,z]) == 'edges1' ((x,y) ':|' [(x,z), (y,z)])
-- clique1 (xs '<>' ys)   == 'connect' (clique1 xs) (clique1 ys)
-- clique1 . 'Data.List.NonEmpty.reverse'    == 'transpose' . clique1
-- @
clique1 :: NonEmpty a -> NonEmptyGraph a
clique1 = connects1 . fmap vertex

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique1 (x1 ':|' [x2]) (y1 ':|' [y2]) == 'edges1' ((x1,y1) ':|' [(x1,y2), (x2,y1), (x2,y2)])
-- biclique1 xs            ys          == 'connect' ('vertices1' xs) ('vertices1' ys)
-- @
biclique1 :: NonEmpty a -> NonEmpty a -> NonEmptyGraph a
biclique1 xs ys = connect (vertices1 xs) (vertices1 ys)

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges1' ((x,y) ':|' [(x,z)])
-- @
star :: a -> [a] -> NonEmptyGraph a
star x []     = vertex x
star x (y:ys) = connect (vertex x) (vertices1 $ y :| ys)

-- | The /star transpose/ formed by a list of leaves connected to a centre vertex.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- starTranspose x []    == 'vertex' x
-- starTranspose x [y]   == 'edge' y x
-- starTranspose x [y,z] == 'edges1' ((y,x) ':|' [(z,x)])
-- starTranspose x ys    == 'transpose' ('star' x ys)
-- @
starTranspose :: a -> [a] -> NonEmptyGraph a
starTranspose x []     = vertex x
starTranspose x (y:ys) = connect (vertices1 $ y :| ys) (vertex x)

-- | The /tree graph/ constructed from a given 'Tree.Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree (i.e. the number of vertices in the tree).
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path1' (x ':|' [y,z])
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges1' ((1,2) ':|' [(1,3), (3,4), (3,5)])
-- @
tree :: Tree.Tree a -> NonEmptyGraph a
tree (Tree.Node x f) = overlays1 $ star x (map Tree.rootLabel f) :| map tree f

-- | Construct a /mesh graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh1 (x ':|' [])    (y ':|' [])    == 'vertex' (x, y)
-- mesh1 xs           ys           == 'box' ('path1' xs) ('path1' ys)
-- mesh1 (1 ':|' [2,3]) (\'a\' ':|' "b") == 'edges1' ('Data.List.NonEmpty.fromList' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\'))
--                                                     , ((1,\'b\'),(2,\'b\')), ((2,\'a\'),(2,\'b\'))
--                                                     , ((2,\'a\'),(3,\'a\')), ((2,\'b\'),(3,\'b\'))
--                                                     , ((3,\'a\'),(3,\'b\')) ])
-- @
mesh1 :: NonEmpty a -> NonEmpty b -> NonEmptyGraph (a, b)
mesh1 xs ys = path1 xs `box` path1 ys

-- | Construct a /torus graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus1 (x ':|' [])  (y ':|' [])    == 'edge' (x, y) (x, y)
-- torus1 xs         ys           == 'box' ('circuit1' xs) ('circuit1' ys)
-- torus1 (1 ':|' [2]) (\'a\' ':|' "b") == 'edges1' ('Data.List.NonEmpty.fromList' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\'))
--                                                    , ((1,\'b\'),(1,\'a\')), ((1,\'b\'),(2,\'b\'))
--                                                    , ((2,\'a\'),(1,\'a\')), ((2,\'a\'),(2,\'b\'))
--                                                    , ((2,\'b\'),(1,\'b\')), ((2,\'b\'),(2,\'a\')) ])
-- @
torus1 :: NonEmpty a -> NonEmpty b -> NonEmptyGraph (a, b)
torus1 xs ys = circuit1 xs `box` circuit1 ys

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
{-# SPECIALISE removeVertex1 :: Int -> NonEmptyGraph Int -> Maybe (NonEmptyGraph Int) #-}
removeVertex1 :: Eq a => a -> NonEmptyGraph a -> Maybe (NonEmptyGraph a)
removeVertex1 x = induce1 (/= x)

-- | Remove an edge from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices1' (x ':|' [y])
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- 'size' (removeEdge x y z)         <= 3 * 'size' z
-- @
{-# SPECIALISE removeEdge :: Int -> Int -> NonEmptyGraph Int -> NonEmptyGraph Int #-}
removeEdge :: Eq a => a -> a -> NonEmptyGraph a -> NonEmptyGraph a
removeEdge s t = filterContext s (/=s) (/=t)

-- TODO: Export
{-# SPECIALISE filterContext :: Int -> (Int -> Bool) -> (Int -> Bool) -> NonEmptyGraph Int -> NonEmptyGraph Int #-}
filterContext :: Eq a => a -> (a -> Bool) -> (a -> Bool) -> NonEmptyGraph a -> NonEmptyGraph a
filterContext s i o g = maybe g go $ G.context (==s) (T.toGraph g)
  where
    go (G.Context is os) = G.induce (/=s) (T.toGraph g)  `overlay1`
                           starTranspose s (filter i is) `overlay` star s (filter o os)

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'NonEmptyGraph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
{-# SPECIALISE replaceVertex :: Int -> Int -> NonEmptyGraph Int -> NonEmptyGraph Int #-}
replaceVertex :: Eq a => a -> a -> NonEmptyGraph a -> NonEmptyGraph a
replaceVertex u v = fmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> NonEmptyGraph a -> NonEmptyGraph a
mergeVertices p v = fmap $ \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
-- Complexity: /O(s + k * L)/ time, memory and size, where /k/ is the number of
-- occurrences of the vertex in the expression and /L/ is the length of the
-- given list.
--
-- @
-- splitVertex1 x (x ':|' [] )               == id
-- splitVertex1 x (y ':|' [] )               == 'replaceVertex' x y
-- splitVertex1 1 (0 ':|' [1]) $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
{-# SPECIALISE splitVertex1 :: Int -> NonEmpty Int -> NonEmptyGraph Int -> NonEmptyGraph Int #-}
splitVertex1 :: Eq a => a -> NonEmpty a -> NonEmptyGraph a -> NonEmptyGraph a
splitVertex1 v us g = g >>= \w -> if w == v then vertices1 us else vertex w

-- | Transpose a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- transpose ('box' x y)   == 'box' (transpose x) (transpose y)
-- 'edgeList' . transpose  == 'Data.List.sort' . map 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: NonEmptyGraph a -> NonEmptyGraph a
transpose = foldg1 vertex overlay (flip connect)

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate. Returns @Nothing@ if the
-- resulting graph is empty.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- induce1 (const True ) x == Just x
-- induce1 (const False) x == Nothing
-- induce1 (/= x)          == 'removeVertex1' x
-- induce1 p '>=>' induce1 q == induce1 (\\x -> p x && q x)
-- @
induce1 :: (a -> Bool) -> NonEmptyGraph a -> Maybe (NonEmptyGraph a)
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
-- simplify              == id
-- 'size' (simplify x)     <= 'size' x
-- simplify 1           '===' 1
-- simplify (1 + 1)     '===' 1
-- simplify (1 + 2 + 1) '===' 1 + 2
-- simplify (1 * 1 * 1) '===' 1 * 1
-- @
{-# SPECIALISE simplify :: NonEmptyGraph Int -> NonEmptyGraph Int #-}
simplify :: Ord a => NonEmptyGraph a -> NonEmptyGraph a
simplify = foldg1 Vertex (simple Overlay) (simple Connect)

{-# SPECIALISE simple :: (NonEmptyGraph Int -> NonEmptyGraph Int -> NonEmptyGraph Int) -> NonEmptyGraph Int -> NonEmptyGraph Int -> NonEmptyGraph Int #-}
simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O(s1 * s2)/ time, memory and size, where /s1/ and /s2/ are the
-- sizes of the given graphs.
--
-- @
-- box ('path1' $ 'Data.List.NonEmpty.fromList' [0,1]) ('path1' $ 'Data.List.NonEmpty.fromList' "ab") == 'edges1' ('Data.List.NonEmpty.fromList' [ ((0,\'a\'), (0,\'b\'))
--                                                                          , ((0,\'a\'), (1,\'a\'))
--                                                                          , ((0,\'b\'), (1,\'b\'))
--                                                                          , ((1,\'a\'), (1,\'b\')) ])
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
box :: NonEmptyGraph a -> NonEmptyGraph b -> NonEmptyGraph (a, b)
box x y = overlays1 xs `overlay` overlays1 ys
  where
    xs = fmap (\b -> fmap (,b) x) $ toNonEmpty y
    ys = fmap (\a -> fmap (a,) y) $ toNonEmpty x

-- Shall we export this? I suggest to wait for Foldable1 type class instead.
toNonEmpty :: NonEmptyGraph a -> NonEmpty a
toNonEmpty = foldg1 (:| []) (<>) (<>)
