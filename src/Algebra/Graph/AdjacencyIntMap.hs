-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyIntMap
-- Copyright  : (c) Andrey Mokhov 2016-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'AdjacencyIntMap' data type and associated functions.
-- See "Algebra.Graph.AdjacencyIntMap.Algorithm" for implementations of basic
-- graph algorithms. 'AdjacencyIntMap' is an instance of the 'C.Graph' type
-- class, which can be used for polymorphic graph construction and manipulation.
-- See "Algebra.Graph.AdjacencyMap" for graphs with non-@Int@ vertices.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyIntMap (
    -- * Data structure
    AdjacencyIntMap, adjacencyIntMap, fromAdjacencyMap,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexIntSet, edgeSet, preIntSet, postIntSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, fromAdjacencyIntSets, tree,
    forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, transpose, gmap,
    induce,

    -- * Relational operations
    compose, closure, reflexiveClosure, symmetricClosure, transitiveClosure,

    -- * Miscellaneous
    consistent
    ) where

import Control.DeepSeq
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.List ((\\))
import Data.Monoid (Sum (..))
import Data.Set (Set)
import Data.Tree
import GHC.Generics

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set

import qualified Algebra.Graph.AdjacencyMap as AM

{-| The 'AdjacencyIntMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a 'Num' instance as a convenient notation for
working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

__Note:__ the 'Num' instance does not satisfy several "customary laws" of 'Num',
which dictate that 'fromInteger' @0@ and 'fromInteger' @1@ should act as
additive and multiplicative identities, and 'negate' as additive inverse.
Nevertheless, overloading 'fromInteger', '+' and '*' is very convenient when
working with algebraic graphs; we hope that in future Haskell's Prelude will
provide a more fine-grained class hierarchy for algebraic structures, which we
would be able to utilise without violating any laws.

The 'Show' instance is defined using basic graph construction primitives:

@show (empty     :: AdjacencyIntMap Int) == "empty"
show (1         :: AdjacencyIntMap Int) == "vertex 1"
show (1 + 2     :: AdjacencyIntMap Int) == "vertices [1,2]"
show (1 * 2     :: AdjacencyIntMap Int) == "edge 1 2"
show (1 * 2 * 3 :: AdjacencyIntMap Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: AdjacencyIntMap Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance satisfies all axioms of algebraic graphs:

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

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.

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
'edge' 1 2 < 'edge' 1 3@

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'isSubgraphOf' x y ==> x <= y@

@'empty' <= x
x     <= x + y
x + y <= x * y@
-}
newtype AdjacencyIntMap = AM {
    -- | The /adjacency map/ of a graph: each vertex is associated with a set of
    -- its direct successors. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- adjacencyIntMap 'empty'      == IntMap.'IntMap.empty'
    -- adjacencyIntMap ('vertex' x) == IntMap.'IntMap.singleton' x IntSet.'IntSet.empty'
    -- adjacencyIntMap ('edge' 1 1) == IntMap.'IntMap.singleton' 1 (IntSet.'IntSet.singleton' 1)
    -- adjacencyIntMap ('edge' 1 2) == IntMap.'IntMap.fromList' [(1,IntSet.'IntSet.singleton' 2), (2,IntSet.'IntSet.empty')]
    -- @
    adjacencyIntMap :: IntMap IntSet } deriving (Eq, Generic)

instance Show AdjacencyIntMap where
    showsPrec p am@(AM m)
        | null vs    = showString "empty"
        | null es    = showParen (p > 10) $ vshow vs
        | vs == used = showParen (p > 10) $ eshow es
        | otherwise  = showParen (p > 10) $
                           showString "overlay (" . vshow (vs \\ used) .
                           showString ") (" . eshow es . showString ")"
      where
        vs             = vertexList am
        es             = edgeList am
        vshow [x]      = showString "vertex "   . showsPrec 11 x
        vshow xs       = showString "vertices " . showsPrec 11 xs
        eshow [(x, y)] = showString "edge "     . showsPrec 11 x .
                         showString " "         . showsPrec 11 y
        eshow xs       = showString "edges "    . showsPrec 11 xs
        used           = IntSet.toAscList (referredToVertexSet m)

instance Ord AdjacencyIntMap where
    compare x y = mconcat
        [ compare (vertexCount  x) (vertexCount  y)
        , compare (vertexIntSet x) (vertexIntSet y)
        , compare (edgeCount    x) (edgeCount    y)
        , compare (edgeSet      x) (edgeSet      y) ]

-- | __Note:__ this does not satisfy the usual ring laws; see 'AdjacencyIntMap'
-- for more details.
instance Num AdjacencyIntMap where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance NFData AdjacencyIntMap where
    rnf (AM a) = rnf a

-- | Construct an 'AdjacencyIntMap' from an 'AM.AdjacencyMap' with vertices of
-- type 'Int'.
-- Complexity: /O(n + m)/ time and memory.
--
-- @
-- fromAdjacencyMap == 'stars' . AdjacencyMap.'AM.adjacencyList'
-- @
fromAdjacencyMap :: AM.AdjacencyMap Int -> AdjacencyIntMap
fromAdjacencyMap = AM
                 . IntMap.fromAscList
                 . map (fmap $ IntSet.fromAscList . Set.toAscList)
                 . Map.toAscList
                 . AM.adjacencyMap

-- | Construct the /empty graph/.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: AdjacencyIntMap
empty = AM IntMap.empty
{-# NOINLINE [1] empty #-}

-- | Construct the graph comprising /a single isolated vertex/.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex y) == (x == y)
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: Int -> AdjacencyIntMap
vertex x = AM $ IntMap.singleton x IntSet.empty
{-# NOINLINE [1] vertex #-}

-- | Construct the graph comprising /a single edge/.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Int -> Int -> AdjacencyIntMap
edge x y | x == y    = AM $ IntMap.singleton x (IntSet.singleton y)
         | otherwise = AM $ IntMap.fromList [(x, IntSet.singleton y), (y, IntSet.empty)]

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
overlay :: AdjacencyIntMap -> AdjacencyIntMap -> AdjacencyIntMap
overlay (AM x) (AM y) = AM $ IntMap.unionWith IntSet.union x y
{-# NOINLINE [1] overlay #-}

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
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
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- @
connect :: AdjacencyIntMap -> AdjacencyIntMap -> AdjacencyIntMap
connect (AM x) (AM y) = AM $ IntMap.unionsWith IntSet.union
    [ x, y, IntMap.fromSet (const $ IntMap.keysSet y) (IntMap.keysSet x) ]
{-# NOINLINE [1] connect #-}

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []             == 'empty'
-- vertices [x]            == 'vertex' x
-- 'hasVertex' x  . vertices == 'elem' x
-- 'vertexCount'  . vertices == 'length' . 'Data.List.nub'
-- 'vertexIntSet' . vertices == IntSet.'IntSet.fromList'
-- @
vertices :: [Int] -> AdjacencyIntMap
vertices = AM . IntMap.fromList . map (\x -> (x, IntSet.empty))
{-# NOINLINE [1] vertices #-}

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'edge' x y
-- edges             == 'overlays' . 'map' ('uncurry' 'edge')
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- 'edgeList' . edges  == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: [(Int, Int)] -> AdjacencyIntMap
edges = fromAdjacencyIntSets . map (fmap IntSet.singleton)

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: [AdjacencyIntMap] -> AdjacencyIntMap
overlays = AM . IntMap.unionsWith IntSet.union . map adjacencyIntMap
{-# NOINLINE [1] overlays #-}

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: [AdjacencyIntMap] -> AdjacencyIntMap
connects  = foldr connect empty
{-# NOINLINE [1] connects #-}

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- isSubgraphOf 'empty'         x             ==  True
-- isSubgraphOf ('vertex' x)    'empty'         ==  False
-- isSubgraphOf x             ('overlay' x y) ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: AdjacencyIntMap -> AdjacencyIntMap -> Bool
isSubgraphOf (AM x) (AM y) = IntMap.isSubmapOfBy IntSet.isSubsetOf x y

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: AdjacencyIntMap -> Bool
isEmpty = IntMap.null . adjacencyIntMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' y)       == (x == y)
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Int -> AdjacencyIntMap -> Bool
hasVertex x = IntMap.member x . adjacencyIntMap

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Int -> Int -> AdjacencyIntMap -> Bool
hasEdge u v (AM m) = case IntMap.lookup u m of
    Nothing -> False
    Just vs -> IntSet.member v vs

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: AdjacencyIntMap -> Int
vertexCount = IntMap.size . adjacencyIntMap

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyIntMap -> Int
edgeCount = getSum . foldMap (Sum . IntSet.size) . adjacencyIntMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: AdjacencyIntMap -> [Int]
vertexList = IntMap.keys . adjacencyIntMap

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- edgeList . 'transpose'    == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . edgeList
-- @
edgeList :: AdjacencyIntMap -> [(Int, Int)]
edgeList (AM m) = [ (x, y) | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]
{-# INLINE edgeList #-}

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexIntSet 'empty'      == IntSet.'IntSet.empty'
-- vertexIntSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexIntSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexIntSet :: AdjacencyIntMap -> IntSet
vertexIntSet = IntMap.keysSet . adjacencyIntMap

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: AdjacencyIntMap -> Set (Int, Int)
edgeSet = Set.fromAscList . edgeList

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and memory.
--
-- @
-- adjacencyList 'empty'          == []
-- adjacencyList ('vertex' x)     == [(x, [])]
-- adjacencyList ('edge' 1 2)     == [(1, [2]), (2, [])]
-- adjacencyList ('star' 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]
-- 'stars' . adjacencyList        == id
-- @
adjacencyList :: AdjacencyIntMap -> [(Int, [Int])]
adjacencyList = map (fmap IntSet.toAscList) . IntMap.toAscList . adjacencyIntMap

-- | The /preset/ (here @preIntSet@) of an element @x@ is the set of its
-- /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preIntSet x 'empty'      == Set.'Set.empty'
-- preIntSet x ('vertex' x) == Set.'Set.empty'
-- preIntSet 1 ('edge' 1 2) == Set.'Set.empty'
-- preIntSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preIntSet :: Int -> AdjacencyIntMap -> IntSet.IntSet
preIntSet x = IntSet.fromAscList . map fst . filter p  . IntMap.toAscList . adjacencyIntMap
  where
    p (_, set) = x `IntSet.member` set

-- | The /postset/ (here @postIntSet@) of a vertex is the set of its
-- /direct successors/.
--
-- @
-- postIntSet x 'empty'      == IntSet.'IntSet.empty'
-- postIntSet x ('vertex' x) == IntSet.'IntSet.empty'
-- postIntSet x ('edge' x y) == IntSet.'IntSet.fromList' [y]
-- postIntSet 2 ('edge' 1 2) == IntSet.'IntSet.empty'
-- @
postIntSet :: Int -> AdjacencyIntMap -> IntSet
postIntSet x = IntMap.findWithDefault IntSet.empty x . adjacencyIntMap

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path :: [Int] -> AdjacencyIntMap
path xs = case xs of []     -> empty
                     [x]    -> vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edges' [(x,y), (y,x)]
-- circuit . 'reverse' == 'transpose' . circuit
-- @
circuit :: [Int] -> AdjacencyIntMap
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique . 'reverse'  == 'transpose' . clique
-- @
clique :: [Int] -> AdjacencyIntMap
clique = fromAdjacencyIntSets . fst . go
  where
    go []     = ([], IntSet.empty)
    go (x:xs) = let (res, set) = go xs in ((x, set) : res, IntSet.insert x set)
{-# NOINLINE [1] clique #-}

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: [Int] -> [Int] -> AdjacencyIntMap
biclique xs ys = AM $ IntMap.fromSet adjacent (x `IntSet.union` y)
  where
    x = IntSet.fromList xs
    y = IntSet.fromList ys
    adjacent v = if v `IntSet.member` x then y else IntSet.empty

-- TODO: Optimise.
-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: Int -> [Int] -> AdjacencyIntMap
star x [] = vertex x
star x ys = connect (vertex x) (vertices ys)
{-# INLINE star #-}

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L * log(n))/ time, memory and size, where /L/ is the total
-- size of the input.
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
stars :: [(Int, [Int])] -> AdjacencyIntMap
stars = fromAdjacencyIntSets . map (fmap IntSet.fromList)

-- | Construct a graph from a list of adjacency sets; a variation of 'stars'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyIntSets []                                     == 'empty'
-- fromAdjacencyIntSets [(x, IntSet.'IntSet.empty')]                    == 'vertex' x
-- fromAdjacencyIntSets [(x, IntSet.'IntSet.singleton' y)]              == 'edge' x y
-- fromAdjacencyIntSets . 'map' ('fmap' IntSet.'IntSet.fromList')           == 'stars'
-- 'overlay' (fromAdjacencyIntSets xs) (fromAdjacencyIntSets ys) == fromAdjacencyIntSets (xs ++ ys)
-- @
fromAdjacencyIntSets :: [(Int, IntSet)] -> AdjacencyIntMap
fromAdjacencyIntSets ss = AM $ IntMap.unionWith IntSet.union vs es
  where
    vs = IntMap.fromSet (const IntSet.empty) . IntSet.unions $ map snd ss
    es = IntMap.fromListWith IntSet.union ss

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Tree Int -> AdjacencyIntMap
tree (Node x []) = vertex x
tree (Node x f ) = star x (map rootLabel f)
    `overlay` forest (filter (not . null . subForest) f)

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . 'map' 'tree'
-- @
forest :: Forest Int -> AdjacencyIntMap
forest = overlays . map tree

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Int -> AdjacencyIntMap -> AdjacencyIntMap
removeVertex x = AM . IntMap.map (IntSet.delete x) . IntMap.delete x . adjacencyIntMap

-- | Remove an edge from a given graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Int -> Int -> AdjacencyIntMap -> AdjacencyIntMap
removeEdge x y = AM . IntMap.adjust (IntSet.delete y) x . adjacencyIntMap

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyIntMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Int -> Int -> AdjacencyIntMap -> AdjacencyIntMap
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- constant time.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (Int -> Bool) -> Int -> AdjacencyIntMap -> AdjacencyIntMap
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- 'edgeList' . transpose  == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: AdjacencyIntMap -> AdjacencyIntMap
transpose (AM m) = AM $ IntMap.foldrWithKey combine vs m
  where
    combine v es = IntMap.unionWith IntSet.union (IntMap.fromSet (const $ IntSet.singleton v) es)
    vs           = IntMap.fromSet (const IntSet.empty) (IntMap.keysSet m)
{-# NOINLINE [1] transpose #-}

{-# RULES
"transpose/empty"    transpose empty = empty
"transpose/vertex"   forall x. transpose (vertex x) = vertex x
"transpose/overlay"  forall g1 g2. transpose (overlay g1 g2) = overlay (transpose g1) (transpose g2)
"transpose/connect"  forall g1 g2. transpose (connect g1 g2) = connect (transpose g2) (transpose g1)

"transpose/overlays" forall xs. transpose (overlays xs) = overlays (map transpose xs)
"transpose/connects" forall xs. transpose (connects xs) = connects (reverse (map transpose xs))

"transpose/vertices" forall xs. transpose (vertices xs) = vertices xs
"transpose/clique"   forall xs. transpose (clique xs)   = clique (reverse xs)
 #-}

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyIntMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Int -> Int) -> AdjacencyIntMap -> AdjacencyIntMap
gmap f = AM . IntMap.map (IntSet.map f) . IntMap.mapKeysWith IntSet.union f . adjacencyIntMap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(n + m)/ time, assuming that the predicate takes constant time.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (Int -> Bool) -> AdjacencyIntMap -> AdjacencyIntMap
induce p = AM . IntMap.map (IntSet.filter p) . IntMap.filterWithKey (\k _ -> p k) . adjacencyIntMap

-- | Left-to-right /relational composition/ of graphs: vertices @x@ and @z@ are
-- connected in the resulting graph if there is a vertex @y@, such that @x@ is
-- connected to @y@ in the first graph, and @y@ is connected to @z@ in the
-- second graph. There are no isolated vertices in the result. This operation is
-- associative, has 'empty' and single-'vertex' graphs as /annihilating zeroes/,
-- and distributes over 'overlay'.
-- Complexity: /O(n * m * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- compose 'empty'            x                == 'empty'
-- compose x                'empty'            == 'empty'
-- compose ('vertex' x)       y                == 'empty'
-- compose x                ('vertex' y)       == 'empty'
-- compose x                (compose y z)    == compose (compose x y) z
-- compose x                ('overlay' y z)    == 'overlay' (compose x y) (compose x z)
-- compose ('overlay' x y)    z                == 'overlay' (compose x z) (compose y z)
-- compose ('edge' x y)       ('edge' y z)       == 'edge' x z
-- compose ('path'    [1..5]) ('path'    [1..5]) == 'edges' [(1,3), (2,4), (3,5)]
-- compose ('circuit' [1..5]) ('circuit' [1..5]) == 'circuit' [1,3,5,2,4]
-- @
compose :: AdjacencyIntMap -> AdjacencyIntMap -> AdjacencyIntMap
compose x y = fromAdjacencyIntSets
    [ (t, ys) | v <- IntSet.toList vs, let ys = postIntSet v y
              , not (IntSet.null ys), t <- IntSet.toList (postIntSet v tx) ]
  where
    tx = transpose x
    vs = vertexIntSet x `IntSet.union` vertexIntSet y

-- | Compute the /reflexive and transitive closure/ of a graph.
-- Complexity: /O(n * m * log(n)^2)/ time.
--
-- @
-- closure 'empty'            == 'empty'
-- closure ('vertex' x)       == 'edge' x x
-- closure ('edge' x x)       == 'edge' x x
-- closure ('edge' x y)       == 'edges' [(x,x), (x,y), (y,y)]
-- closure ('path' $ 'Data.List.nub' xs) == 'reflexiveClosure' ('clique' $ 'Data.List.nub' xs)
-- closure                  == 'reflexiveClosure' . 'transitiveClosure'
-- closure                  == 'transitiveClosure' . 'reflexiveClosure'
-- closure . closure        == closure
-- 'postIntSet' x (closure y) == IntSet.'IntSet.fromList' ('Algebra.Graph.ToGraph.reachable' x y)
-- @
closure :: AdjacencyIntMap -> AdjacencyIntMap
closure = reflexiveClosure . transitiveClosure

-- | Compute the /reflexive closure/ of a graph by adding a self-loop to every
-- vertex.
-- Complexity: /O(n * log(n))/ time.
--
-- @
-- reflexiveClosure 'empty'              == 'empty'
-- reflexiveClosure ('vertex' x)         == 'edge' x x
-- reflexiveClosure ('edge' x x)         == 'edge' x x
-- reflexiveClosure ('edge' x y)         == 'edges' [(x,x), (x,y), (y,y)]
-- reflexiveClosure . reflexiveClosure == reflexiveClosure
-- @
reflexiveClosure :: AdjacencyIntMap -> AdjacencyIntMap
reflexiveClosure (AM m) = AM $ IntMap.mapWithKey (\k -> IntSet.insert k) m

-- | Compute the /symmetric closure/ of a graph by overlaying it with its own
-- transpose.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- symmetricClosure 'empty'              == 'empty'
-- symmetricClosure ('vertex' x)         == 'vertex' x
-- symmetricClosure ('edge' x y)         == 'edges' [(x,y), (y,x)]
-- symmetricClosure x                  == 'overlay' x ('transpose' x)
-- symmetricClosure . symmetricClosure == symmetricClosure
-- @
symmetricClosure :: AdjacencyIntMap -> AdjacencyIntMap
symmetricClosure m = overlay m (transpose m)

-- | Compute the /transitive closure/ of a graph.
-- Complexity: /O(n * m * log(n)^2)/ time.
--
-- @
-- transitiveClosure 'empty'               == 'empty'
-- transitiveClosure ('vertex' x)          == 'vertex' x
-- transitiveClosure ('edge' x y)          == 'edge' x y
-- transitiveClosure ('path' $ 'Data.List.nub' xs)     == 'clique' ('Data.List.nub' xs)
-- transitiveClosure . transitiveClosure == transitiveClosure
-- @
transitiveClosure :: AdjacencyIntMap -> AdjacencyIntMap
transitiveClosure old
    | old == new = old
    | otherwise  = transitiveClosure new
  where
    new = overlay old (old `compose` old)

-- | Check that the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
--
-- @
-- consistent 'empty'         == True
-- consistent ('vertex' x)    == True
-- consistent ('overlay' x y) == True
-- consistent ('connect' x y) == True
-- consistent ('edge' x y)    == True
-- consistent ('edges' xs)    == True
-- consistent ('stars' xs)    == True
-- @
consistent :: AdjacencyIntMap -> Bool
consistent (AM m) = referredToVertexSet m `IntSet.isSubsetOf` IntMap.keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: IntMap IntSet -> IntSet
referredToVertexSet m = IntSet.fromList $ concat
    [ [x, y] | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]
