-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Undirected
-- Copyright  : (c) Andrey Mokhov 2016-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines an undirected version of algebraic graphs. Undirected
-- graphs satisfy all laws of the 'Algebra.Graph.Class.Undirected' type class,
-- including the commutativity of 'connect'.
--
-- To avoid name clashes with "Algebra.Graph", this module can be imported
-- qualified:
--
-- @
-- import qualified Algebra.Graph.Undirected as Undirected
-- @

-----------------------------------------------------------------------------
module Algebra.Graph.Undirected (
    -- * Algebraic data type for graphs
    Graph, fromUndirected, toUndirected,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Graph folding
    foldg,

    -- * Relations on graphs
    isSubgraphOf, toRelation,

    -- * Graph properties
    isEmpty, size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, edgeSet, adjacencyList, neighbours,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, induce, induceJust,
    complement
    ) where

import Algebra.Graph.Internal
import Algebra.Graph.ToGraph (toGraph)
import Control.Applicative (Alternative)
import Control.DeepSeq
import Control.Monad
import Data.Coerce
import Data.List
import GHC.Generics
import Data.Set (Set)
import Data.Tree (Tree, Forest)
import Data.String

import qualified Algebra.Graph                    as G
import qualified Algebra.Graph.Relation.Symmetric as R
import qualified Data.Set                         as Set

-- TODO: Specialise the API for graphs with vertices of type 'Int'.

{-| The 'Graph' data type provides the four algebraic graph construction
primitives 'empty', 'vertex', 'overlay' and 'connect', as well as various
derived functions. The only difference compared to the 'Algebra.Graph.Graph'
data type defined in "Algebra.Graph" is that the 'connect' operation is
/commutative/. We define a 'Num' instance as a convenient notation for working
with undirected graphs:

    > 0           == vertex 0
    > 1 + 2       == vertices [1,2]
    > 1 * 2       == edge 1 2
    > 1 + 2 * 3   == overlay (vertex 1) (edge 2 3)
    > 1 * (2 + 3) == edges [(1,2),(1,3)]

__Note:__ the 'Num' instance does not satisfy several "customary laws" of 'Num',
which dictate that 'fromInteger' @0@ and 'fromInteger' @1@ should act as
additive and multiplicative identities, and 'negate' as additive inverse.
Nevertheless, overloading 'fromInteger', '+' and '*' is very convenient when
working with algebraic graphs; we hope that in future Haskell's Prelude will
provide a more fine-grained class hierarchy for algebraic structures, which we
would be able to utilise without violating any laws.

The 'Eq' instance is currently implemented using the 'R.Relation' as the
/canonical graph representation/ and satisfies all axioms of algebraic graphs:

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is associative, commutative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        >       x * y == y * x
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
denote the number of vertices in the graph, /m/ will denote the number of edges
in the graph, and /s/ will denote the /size/ of the corresponding 'Graph'
expression. For example, if @g@ is a 'Graph' then /n/, /m/ and /s/ can be
computed as follows:

@n == 'vertexCount' g
m == 'edgeCount' g
s == 'size' g@

Note that 'size' counts all leaves of the expression:

@'vertexCount' 'empty'           == 0
'size'        'empty'           == 1
'vertexCount' ('vertex' x)      == 1
'size'        ('vertex' x)      == 1
'vertexCount' ('empty' + 'empty') == 0
'size'        ('empty' + 'empty') == 2@

Converting an undirected 'Graph' to the corresponding 'R.Relation' takes
/O(s + m * log(m))/ time and /O(s + m)/ memory. This is also the complexity of
the graph equality test, because it is currently implemented by converting graph
expressions to canonical representations based on adjacency maps.

The total order on graphs is defined using /size-lexicographic/ comparison:

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
'edge' 1 2 < 'edge' 1 3
'edge' 1 2 == 'edge' 2 1@

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'isSubgraphOf' x y ==> x <= y@

@'empty' <= x
x     <= x + y
x + y <= x * y@
-}
newtype Graph a = UG (G.Graph a)
    deriving ( Alternative, Applicative, Functor, Generic, IsString, Monad
             , MonadPlus, NFData )

instance (Show a, Ord a) => Show (Graph a) where
    show = show . toRelation

-- | __Note:__ this does not satisfy the usual ring laws; see 'Graph' for more
-- details.
instance Num a => Num (Graph a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Ord a => Eq (Graph a) where
    (==) = eqR

instance Ord a => Ord (Graph a) where
    compare = ordR

-- TODO: Find a more efficient equality check.
-- Check if two graphs are equal by converting them to symmetric relations.
eqR :: Ord a => Graph a -> Graph a -> Bool
eqR x y = toRelation x == toRelation y

-- TODO: Find a more efficient comparison.
-- Compare two graphs by converting them to their symmetric relations.
ordR :: Ord a => Graph a -> Graph a -> Ordering
ordR x y = compare (toRelation x) (toRelation y)

-- | Construct an undirected graph from a given "Algebra.Graph".
-- Complexity: /O(1)/ time.
--
-- @
-- toUndirected ('Algebra.Graph.edge' 1 2)         == 'edge' 1 2
-- toUndirected . 'fromUndirected'   == id
-- 'vertexCount' . toUndirected      == 'Algebra.Graph.vertexCount'
-- (*2) . 'edgeCount' . toUndirected >= 'Algebra.Graph.edgeCount'
-- @
toUndirected :: G.Graph a -> Graph a
toUndirected = coerce

-- | Extract the underlying "Algebra.Graph".
-- Complexity: /O(n + m)/ time.
--
-- @
-- fromUndirected ('Algebra.Graph.edge' 1 2)     == 'Algebra.Graph.edges' [(1,2),(2,1)]
-- 'toUndirected' . 'fromUndirected' == id
-- 'Algebra.Graph.vertexCount' . fromUndirected  == 'vertexCount'
-- 'Algebra.Graph.edgeCount' . fromUndirected    <= (*2) . 'edgeCount'
-- @
fromUndirected :: Ord a => Graph a -> G.Graph a
fromUndirected = toGraph . toRelation

-- | Construct the /empty graph/.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- 'size'        empty == 1
-- @
empty :: Graph a
empty = coerce00 G.empty
{-# INLINE empty #-}

-- | Construct the graph comprising /a single isolated vertex/.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex y) == (x == y)
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- 'size'        (vertex x) == 1
-- @
vertex :: a -> Graph a
vertex = coerce10 G.vertex
{-# INLINE vertex #-}

-- | Construct the graph comprising /a single edge/.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- edge x y               == 'edge' y x
-- edge x y               == 'edges' [(x,y), (y,x)]
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: a -> a -> Graph a
edge = coerce20 G.edge
{-# INLINE edge #-}

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
overlay :: Graph a -> Graph a -> Graph a
overlay = coerce20 G.overlay
{-# INLINE overlay #-}

-- | /Connect/ two graphs. This is a commutative and associative operation with
-- the identity 'empty', which distributes over 'overlay' and obeys the
-- decomposition axiom.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'connect' x y               == 'connect' y x
-- 'isEmpty'     (connect x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y) >= 'vertexCount' x
-- 'vertexCount' (connect x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'edgeCount' x
-- 'edgeCount'   (connect x y) >= 'edgeCount' y
-- 'edgeCount'   (connect x y) >= 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'vertexCount' x * 'vertexCount' y `div` 2
-- 'size'        (connect x y) == 'size' x        + 'size' y
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- @
connect :: Graph a -> Graph a -> Graph a
connect = coerce20 G.connect
{-# INLINE connect #-}

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set . 'Set.fromList'
-- @
vertices :: [a] -> Graph a
vertices = coerce10 G.vertices
{-# INLINE vertices #-}

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []             == 'empty'
-- edges [(x,y)]        == 'edge' x y
-- edges [(x,y), (y,x)] == 'edge' x y
-- @
edges :: [(a, a)] -> Graph a
edges = coerce10 G.edges
{-# INLINE edges #-}

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
overlays :: [Graph a] -> Graph a
overlays = coerce10 G.overlays
{-# INLINE overlays #-}

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
-- connects           == connects . 'reverse'
-- @
connects :: [Graph a] -> Graph a
connects = coerce10 G.connects
{-# INLINE connects #-}

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, vertex, overlay and connect.
-- Complexity: /O(s)/ applications of the given functions. As an example, the
-- complexity of 'size' is /O(s)/, since 'const' and '+' have constant costs.
--
-- @
-- foldg 'empty' 'vertex'        'overlay' 'connect'        == id
-- foldg 'empty' 'vertex'        'overlay' ('flip' 'connect') == id
-- foldg 1     ('const' 1)     (+)     (+)            == 'size'
-- foldg True  ('const' False) (&&)    (&&)           == 'isEmpty'
-- foldg False (== x)        (||)    (||)           == 'hasVertex' x
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg = coerce G.foldg
  where
    coerce :: (b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> G.Graph a -> b)
           -> (b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) ->   Graph a -> b)
    coerce = Data.Coerce.coerce
{-# INLINE foldg #-}

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
-- isSubgraphOf ('path' xs)     ('circuit' xs)  ==  True
-- isSubgraphOf ('edge' x y)    ('edge' y x)    ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: Ord a => Graph a -> Graph a -> Bool
isSubgraphOf x y = R.isSubgraphOf (toRelation x) (toRelation y)
{-# NOINLINE [1] isSubgraphOf #-}

-- TODO: This is a very inefficient implementation. Find a way to construct a
-- symmetric relation directly, without building intermediate representations
-- for all subgraphs.
-- | Convert an undirected graph to a symmetric 'R.Relation'.
toRelation :: Ord a => Graph a -> R.Relation a
toRelation = foldg R.empty R.vertex R.overlay R.connect
{-# INLINE toRelation #-}

-- | Check if a graph is empty.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: Graph a -> Bool
isEmpty = coerce01 G.isEmpty
{-# INLINE isEmpty #-}

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
size :: Graph a -> Int
size = coerce01 G.size
{-# INLINE size #-}

-- | Check if a graph contains a given vertex.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' y)       == (x == y)
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex = coerce11 G.hasVertex
{-# INLINE hasVertex #-}
{-# SPECIALISE hasVertex :: Int -> Graph Int -> Bool #-}

-- TODO: Optimise this further.
-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y ('edge' y x)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (min x y, max x y) . 'edgeList'
-- @
hasEdge :: Eq a => a -> a -> Graph a -> Bool
hasEdge s t (UG g) = G.hasEdge s t g || G.hasEdge t s g
{-# INLINE hasEdge #-}
{-# SPECIALISE hasEdge :: Int -> Int -> Graph Int -> Bool #-}

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: Ord a => Graph a -> Int
vertexCount = coerce01 G.vertexCount
{-# INLINE [1] vertexCount #-}

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
edgeCount :: Ord a => Graph a -> Int
edgeCount = R.edgeCount . toRelation
{-# INLINE [1] edgeCount #-}

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Ord a => Graph a -> [a]
vertexList = coerce01 G.vertexList
{-# INLINE [1] vertexList #-}

-- | The sorted list of edges of a graph.
-- Complexity: /O(s + m * log(m))/ time and /O(m)/ memory. Note that the number of
-- edges /m/ of a graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(min x y, max y x)]
-- edgeList ('star' 2 [3,1]) == [(1,2), (2,3)]
-- @
edgeList :: Ord a => Graph a -> [(a, a)]
edgeList = R.edgeList . toRelation
{-# INLINE [1] edgeList #-}

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: Ord a => Graph a -> Set a
vertexSet = coerce01 G.vertexSet
{-# INLINE vertexSet #-}

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' ('min' x y, 'max' x y)
-- @
edgeSet :: Ord a => Graph a -> Set (a, a)
edgeSet = R.edgeSet . toRelation
{-# INLINE [1] edgeSet #-}

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and memory.
--
-- @
-- adjacencyList 'empty'          == []
-- adjacencyList ('vertex' x)     == [(x, [])]
-- adjacencyList ('edge' 1 2)     == [(1, [2]), (2, [1])]
-- adjacencyList ('star' 2 [3,1]) == [(1, [2]), (2, [1,3]), (3, [2])]
-- 'stars' . adjacencyList        == id
-- @
adjacencyList :: Ord a => Graph a -> [(a, [a])]
adjacencyList = R.adjacencyList . toRelation
{-# INLINE adjacencyList #-}
{-# SPECIALISE adjacencyList :: Graph Int -> [(Int, [Int])] #-}

-- | The set of vertices /adjacent/ to a given vertex.
--
-- @
-- neighbours x 'empty'      == Set.'Set.empty'
-- neighbours x ('vertex' x) == Set.'Set.empty'
-- neighbours x ('edge' x y) == Set.'Set.fromList' [y]
-- neighbours y ('edge' x y) == Set.'Set.fromList' [x]
-- @
neighbours :: Ord a => a -> Graph a -> Set a
neighbours x = R.neighbours x . toRelation
{-# INLINE neighbours #-}

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == path
-- @
path :: [a] -> Graph a
path = coerce10 G.path
{-# INLINE path #-}

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edge' (x,y)
-- circuit . 'reverse' == circuit
-- @
circuit :: [a] -> Graph a
circuit = coerce10 G.circuit
{-# INLINE circuit #-}

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
-- clique . 'reverse'  == clique
-- @
clique :: [a] -> Graph a
clique = coerce10 G.clique
{-# INLINE clique #-}

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,x2), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: [a] -> [a] -> Graph a
biclique = coerce20 G.biclique
{-# INLINE biclique #-}

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
star :: a -> [a] -> Graph a
star = coerce20 G.star
{-# INLINE star #-}

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the total size of the
-- input.
--
-- @
-- stars []                      == 'empty'
-- stars [(x, [])]               == 'vertex' x
-- stars [(x, [y])]              == 'edge' x y
-- stars [(x, ys)]               == 'star' x ys
-- stars                         == 'overlays' . 'map' ('uncurry' 'star')
-- stars . 'adjacencyList'         == id
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: [(a, [a])] -> Graph a
stars = coerce10 G.stars
{-# INLINE stars #-}

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree (i.e. the number of vertices in the tree).
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Tree a -> Graph a
tree = coerce10 G.tree
{-# INLINE tree #-}

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest (i.e. the number of vertices in the forest).
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . 'map' 'tree'
-- @
forest :: Forest a -> Graph a
forest = coerce10 G.forest
{-# INLINE forest #-}

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
removeVertex :: Eq a => a -> Graph a -> Graph a
removeVertex = coerce11 G.removeVertex
{-# INLINE removeVertex #-}
{-# SPECIALISE removeVertex :: Int -> Graph Int -> Graph Int #-}

-- TODO: Optimise by doing a single graph traversal.
-- | Remove an edge from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y                  == removeEdge y x
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge s t = Data.Coerce.coerce $ G.removeEdge s t . G.removeEdge t s
{-# INLINE removeEdge #-}
{-# SPECIALISE removeEdge :: Int -> Int -> Graph Int -> Graph Int #-}

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Eq a => a -> a -> Graph a -> Graph a
replaceVertex = coerce21 G.replaceVertex
{-# INLINE replaceVertex #-}
{-# SPECIALISE replaceVertex :: Int -> Int -> Graph Int -> Graph Int #-}

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- constant time.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> Graph a -> Graph a
mergeVertices = coerce21 G.mergeVertices
{-# INLINE mergeVertices #-}

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
induce :: (a -> Bool) -> Graph a -> Graph a
induce = coerce20 G.induce
{-# INLINE induce #-}

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
induceJust :: Graph (Maybe a) -> Graph a
induceJust = coerce10 G.induceJust
{-# INLINE induceJust #-}

-- | The edge complement of a graph. Note that, as can be seen from the examples
-- below, this operation ignores self-loops.
-- Complexity: /O(n^2 * log n)/ time, /O(n^2)/ memory.
--
-- @
-- complement 'empty'           == 'empty'
-- complement ('vertex' x)      == ('vertex' x)
-- complement ('edge' 1 2)      == ('vertices' [1, 2])
-- complement ('edge' 0 0)      == ('edge' 0 0)
-- complement ('star' 1 [2, 3]) == ('overlay' ('vertex' 1) ('edge' 2 3))
-- complement . complement    == id
-- @
complement :: Ord a => Graph a -> Graph a
complement g = overlay (vertices vsOld) (edges $ Set.toAscList esNew)
  where
    vsOld = vertexList g
    esOld = edgeSet g
    loops = Set.filter (uncurry (==)) esOld
    esAll = Set.fromAscList [ (x, y) | x:ys <- tails vsOld, y <- ys ]
    esNew = Set.union loops (Set.difference esAll esOld)
