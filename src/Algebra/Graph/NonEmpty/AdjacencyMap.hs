-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.NonEmpty.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the data type 'AdjacencyMap' for graphs that are known
-- to be non-empty at compile time. To avoid name clashes with
-- "Algebra.Graph.AdjacencyMap", this module can be imported qualified:
--
-- @
-- import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
-- @
--
-- The naming convention generally follows that of "Data.List.NonEmpty": we use
-- suffix @1@ to indicate the functions whose interface must be changed compared
-- to "Algebra.Graph.AdjacencyMap", e.g. 'vertices1'.
-----------------------------------------------------------------------------
module Algebra.Graph.NonEmpty.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, toNonEmpty, fromNonEmpty,

    -- * Basic graph construction primitives
    vertex, edge, overlay, connect, vertices1, edges1, overlays1, connects1,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    hasVertex, hasEdge, vertexCount, edgeCount, vertexList1, edgeList,
    vertexSet, edgeSet, preSet, postSet,

    -- * Standard families of graphs
    path1, circuit1, clique1, biclique1, star, stars1, tree,

    -- * Graph transformation
    removeVertex1, removeEdge, replaceVertex, mergeVertices, transpose, gmap,
    induce1, induceJust1,

    -- * Graph closure
    closure, reflexiveClosure, symmetricClosure, transitiveClosure,

    -- * Miscellaneous
    consistent
    ) where

import Prelude hiding (reverse)
import Control.DeepSeq
import Data.Coerce
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList, reverse)
import Data.Maybe
import Data.Set (Set)
import Data.String
import Data.Tree
import GHC.Generics

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Set                   as Set

{-| The 'AdjacencyMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a 'Num' instance as a convenient notation for
working with graphs:

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

The 'Show' instance is defined using basic graph construction primitives:

@show (1         :: AdjacencyMap Int) == "vertex 1"
show (1 + 2     :: AdjacencyMap Int) == "vertices1 [1,2]"
show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"
show (1 * 2 * 3 :: AdjacencyMap Int) == "edges1 [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: AdjacencyMap Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance satisfies the following laws of algebraic graphs:

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

Note that the resulting order refines the
'isSubgraphOf' relation and is compatible
with 'overlay' and
'connect' operations:

@'isSubgraphOf' x y ==> x <= y@

@x     <= x + y
x + y <= x * y@
-}
newtype AdjacencyMap a = NAM { am :: AM.AdjacencyMap a }
    deriving (Eq, Generic, IsString, NFData, Ord)

-- | __Note:__ this does not satisfy the usual ring laws; see 'AdjacencyMap' for
-- more details.
instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = error "NonEmpty.AdjacencyMap.signum cannot be implemented."
    abs         = id
    negate      = id

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    showsPrec p nam
        | null vs    = error "NonEmpty.AdjacencyMap.Show: Graph is empty"
        | null es    = showParen (p > 10) $ vshow vs
        | vs == used = showParen (p > 10) $ eshow es
        | otherwise  = showParen (p > 10) $
                           showString "overlay (" . vshow (vs \\ used) .
                           showString ") (" . eshow es . showString ")"
      where
        vs             = toList (vertexList1 nam)
        es             = edgeList nam
        vshow [x]      = showString "vertex "    . showsPrec 11 x
        vshow xs       = showString "vertices1 " . showsPrec 11 xs
        eshow [(x, y)] = showString "edge "      . showsPrec 11 x .
                         showString " "          . showsPrec 11 y
        eshow xs       = showString "edges1 "    . showsPrec 11 xs
        used           = Set.toAscList $ Set.fromList $ uncurry (++) $ unzip es

-- Unsafe creation of a NonEmpty list.
unsafeNonEmpty :: [a] -> NonEmpty a
unsafeNonEmpty = fromMaybe (error msg) . nonEmpty
  where
    msg = "Algebra.Graph.AdjacencyMap.unsafeNonEmpty: Graph is empty"

-- | Convert a possibly empty 'AM.AdjacencyMap' into NonEmpty.'AdjacencyMap'.
-- Returns 'Nothing' if the argument is 'AM.empty'.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- toNonEmpty 'AM.empty'          == 'Nothing'
-- toNonEmpty . 'fromNonEmpty' == 'Just'
-- @
toNonEmpty :: AM.AdjacencyMap a -> Maybe (AdjacencyMap a)
toNonEmpty x | AM.isEmpty x = Nothing
             | otherwise    = Just (NAM x)

-- | Convert a NonEmpty.'AdjacencyMap' into an 'AM.AdjacencyMap'. The resulting
-- graph is guaranteed to be non-empty.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty' . fromNonEmpty    == 'const' 'False'
-- 'toNonEmpty' . fromNonEmpty == 'Just'
-- @
fromNonEmpty :: AdjacencyMap a -> AM.AdjacencyMap a
fromNonEmpty = am

-- | Construct the graph comprising /a single isolated vertex/.
--
-- @
-- 'hasVertex' x (vertex y) == (x == y)
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap a
vertex = coerce AM.vertex
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
edge :: Ord a => a -> a -> AdjacencyMap a
edge = coerce AM.edge

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay = coerce AM.overlay
{-# NOINLINE [1] overlay #-}

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
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
connect :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect = coerce AM.connect
{-# NOINLINE [1] connect #-}

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices1 [x]           == 'vertex' x
-- 'hasVertex' x . vertices1 == 'elem' x
-- 'vertexCount' . vertices1 == 'length' . 'Data.List.NonEmpty.nub'
-- 'vertexSet'   . vertices1 == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertices1 :: Ord a => NonEmpty a -> AdjacencyMap a
vertices1 = coerce AM.vertices . toList
{-# NOINLINE [1] vertices1 #-}

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges1 [(x,y)]     == 'edge' x y
-- edges1             == 'overlays1' . 'fmap' ('uncurry' 'edge')
-- 'edgeCount' . edges1 == 'Data.List.NonEmpty.length' . 'Data.List.NonEmpty.nub'
-- @
edges1 :: Ord a => NonEmpty (a, a) -> AdjacencyMap a
edges1 = coerce AM.edges . toList

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays1 [x]   == x
-- overlays1 [x,y] == 'overlay' x y
-- @
overlays1 :: Ord a => NonEmpty (AdjacencyMap a) -> AdjacencyMap a
overlays1 = coerce AM.overlays . toList
{-# NOINLINE overlays1 #-}

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects1 [x]   == x
-- connects1 [x,y] == 'connect' x y
-- @
connects1 :: Ord a => NonEmpty (AdjacencyMap a) -> AdjacencyMap a
connects1 = coerce AM.connects . toList
{-# NOINLINE connects1 #-}

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- isSubgraphOf x             ('overlay' x y) ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf ('path1' xs)    ('circuit1' xs) ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: Ord a => AdjacencyMap a -> AdjacencyMap a -> Bool
isSubgraphOf = coerce AM.isSubgraphOf

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x ('vertex' y) == (x == y)
-- @
hasVertex :: Ord a => a -> AdjacencyMap a -> Bool
hasVertex = coerce AM.hasVertex

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap a -> Bool
hasEdge = coerce AM.hasEdge

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: AdjacencyMap a -> Int
vertexCount = coerce AM.vertexCount

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyMap a -> Int
edgeCount = coerce AM.edgeCount

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList1 ('vertex' x)  == [x]
-- vertexList1 . 'vertices1' == 'Data.List.NonEmpty.nub' . 'Data.List.NonEmpty.sort'
-- @
vertexList1 :: AdjacencyMap a -> NonEmpty a
vertexList1 = unsafeNonEmpty . coerce AM.vertexList

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.NonEmpty.nub' . 'Data.List.sort'
-- edgeList . 'transpose'    == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . edgeList
-- @
edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = coerce AM.edgeList

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet . 'vertex'    == Set.'Set.singleton'
-- vertexSet . 'vertices1' == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- vertexSet . 'clique1'   == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertexSet :: AdjacencyMap a -> Set a
vertexSet = coerce AM.vertexSet

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Ord a => AdjacencyMap a -> Set (a, a)
edgeSet = coerce AM.edgeSet

-- | The /preset/ of an element @x@ is the set of its /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preSet x ('vertex' x) == Set.'Set.empty'
-- preSet 1 ('edge' 1 2) == Set.'Set.empty'
-- preSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preSet :: Ord a => a -> AdjacencyMap a -> Set.Set a
preSet = coerce AM.preSet

-- | The /postset/ of a vertex is the set of its /direct successors/.
-- Complexity: /O(log(n))/ time and /O(1)/ memory.
--
-- @
-- postSet x ('vertex' x) == Set.'Set.empty'
-- postSet x ('edge' x y) == Set.'Set.fromList' [y]
-- postSet 2 ('edge' 1 2) == Set.'Set.empty'
-- @
postSet :: Ord a => a -> AdjacencyMap a -> Set a
postSet = coerce AM.postSet

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path1 [x]       == 'vertex' x
-- path1 [x,y]     == 'edge' x y
-- path1 . 'Data.List.NonEmpty.reverse' == 'transpose' . path1
-- @
path1 :: Ord a => NonEmpty a -> AdjacencyMap a
path1 = coerce AM.path . toList

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit1 [x]       == 'edge' x x
-- circuit1 [x,y]     == 'edges1' [(x,y), (y,x)]
-- circuit1 . 'Data.List.NonEmpty.reverse' == 'transpose' . circuit1
-- @
circuit1 :: Ord a => NonEmpty a -> AdjacencyMap a
circuit1 = coerce AM.circuit . toList

-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- clique1 [x]        == 'vertex' x
-- clique1 [x,y]      == 'edge' x y
-- clique1 [x,y,z]    == 'edges1' [(x,y), (x,z), (y,z)]
-- clique1 (xs '<>' ys) == 'connect' (clique1 xs) (clique1 ys)
-- clique1 . 'Data.List.NonEmpty.reverse'  == 'transpose' . clique1
-- @
clique1 :: Ord a => NonEmpty a -> AdjacencyMap a
clique1 = coerce AM.clique . toList
{-# NOINLINE [1] clique1 #-}

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique1 [x1,x2] [y1,y2] == 'edges1' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique1 xs      ys      == 'connect' ('vertices1' xs) ('vertices1' ys)
-- @
biclique1 :: Ord a => NonEmpty a -> NonEmpty a -> AdjacencyMap a
biclique1 xs ys = coerce AM.biclique (toList xs) (toList ys)

-- TODO: Optimise.
-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges1' [(x,y), (x,z)]
-- @
star :: Ord a => a -> [a] -> AdjacencyMap a
star = coerce AM.star
{-# INLINE star #-}

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L * log(n))/ time, memory and size, where /L/ is the total
-- size of the input.
--
-- @
-- stars1 [(x, [] )]               == 'vertex' x
-- stars1 [(x, [y])]               == 'edge' x y
-- stars1 [(x, ys )]               == 'star' x ys
-- stars1                          == 'overlays1' . 'fmap' ('uncurry' 'star')
-- 'overlay' (stars1 xs) (stars1 ys) == stars1 (xs '<>' ys)
-- @
stars1 :: Ord a => NonEmpty (a, [a]) -> AdjacencyMap a
stars1 = coerce AM.stars . toList

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path1' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges1' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Ord a => Tree a -> AdjacencyMap a
tree = coerce AM.tree

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex1 x ('vertex' x)          == Nothing
-- removeVertex1 1 ('vertex' 2)          == Just ('vertex' 2)
-- removeVertex1 x ('edge' x x)          == Nothing
-- removeVertex1 1 ('edge' 1 2)          == Just ('vertex' 2)
-- removeVertex1 x 'Control.Monad.>=>' removeVertex1 x == removeVertex1 x
-- @
removeVertex1 :: Ord a => a -> AdjacencyMap a -> Maybe (AdjacencyMap a)
removeVertex1 = fmap toNonEmpty . coerce AM.removeVertex

-- | Remove an edge from a given graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices1' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
removeEdge = coerce AM.removeEdge

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
replaceVertex = coerce AM.replaceVertex

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
mergeVertices :: Ord a => (a -> Bool) -> a -> AdjacencyMap a -> AdjacencyMap a
mergeVertices = coerce AM.mergeVertices

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- 'edgeList' . transpose  == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: Ord a => AdjacencyMap a -> AdjacencyMap a
transpose = coerce AM.transpose
{-# NOINLINE [1] transpose #-}

{-# RULES
"transpose/vertex"   forall x. transpose (vertex x) = vertex x
"transpose/overlay"  forall g1 g2. transpose (overlay g1 g2) = overlay (transpose g1) (transpose g2)
"transpose/connect"  forall g1 g2. transpose (connect g1 g2) = connect (transpose g2) (transpose g1)

"transpose/overlays1" forall xs. transpose (overlays1 xs) = overlays1 (fmap transpose xs)
"transpose/connects1" forall xs. transpose (connects1 xs) = connects1 (reverse (fmap transpose xs))

"transpose/vertices1" forall xs. transpose (vertices1 xs) = vertices1 xs
"transpose/clique1"   forall xs. transpose (clique1 xs)   = clique1 (reverse xs)
 #-}

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap = coerce AM.gmap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes constant time.
--
-- @
-- induce1 ('const' True ) x == Just x
-- induce1 ('const' False) x == Nothing
-- induce1 (/= x)          == 'removeVertex1' x
-- induce1 p 'Control.Monad.>=>' induce1 q == induce1 (\\x -> p x && q x)
-- @
induce1 :: (a -> Bool) -> AdjacencyMap a -> Maybe (AdjacencyMap a)
induce1 = fmap toNonEmpty . coerce AM.induce

-- | Construct the /induced subgraph/ of a given graph by removing the vertices
-- that are 'Nothing'. Returns 'Nothing' if the resulting graph is empty.
-- Complexity: /O(n + m)/ time.
--
-- @
-- induceJust1 ('vertex' 'Nothing')                               == 'Nothing'
-- induceJust1 ('edge' ('Just' x) 'Nothing')                        == 'Just' ('vertex' x)
-- induceJust1 . 'gmap' 'Just'                                    == 'Just'
-- induceJust1 . 'gmap' (\\x -> if p x then 'Just' x else 'Nothing') == 'induce1' p
-- @
induceJust1 :: Ord a => AdjacencyMap (Maybe a) -> Maybe (AdjacencyMap a)
induceJust1 m = toNonEmpty (AM.induceJust (coerce m))

-- | Compute the /reflexive and transitive closure/ of a graph.
-- Complexity: /O(n * m * log(n)^2)/ time.
--
-- @
-- closure ('vertex' x)       == 'edge' x x
-- closure ('edge' x x)       == 'edge' x x
-- closure ('edge' x y)       == 'edges1' [(x,x), (x,y), (y,y)]
-- closure ('path1' $ 'Data.List.NonEmpty.nub' xs) == 'reflexiveClosure' ('clique1' $ 'Data.List.NonEmpty.nub' xs)
-- closure                  == 'reflexiveClosure' . 'transitiveClosure'
-- closure                  == 'transitiveClosure' . 'reflexiveClosure'
-- closure . closure        == closure
-- 'postSet' x (closure y)    == Set.'Set.fromList' ('Algebra.Graph.ToGraph.reachable' x y)
-- @
closure :: Ord a => AdjacencyMap a -> AdjacencyMap a
closure = coerce AM.closure

-- | Compute the /reflexive closure/ of a graph by adding a self-loop to every
-- vertex.
-- Complexity: /O(n * log(n))/ time.
--
-- @
-- reflexiveClosure ('vertex' x)         == 'edge' x x
-- reflexiveClosure ('edge' x x)         == 'edge' x x
-- reflexiveClosure ('edge' x y)         == 'edges1' [(x,x), (x,y), (y,y)]
-- reflexiveClosure . reflexiveClosure == reflexiveClosure
-- @
reflexiveClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
reflexiveClosure = coerce AM.reflexiveClosure

-- | Compute the /symmetric closure/ of a graph by overlaying it with its own
-- transpose.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- symmetricClosure ('vertex' x)         == 'vertex' x
-- symmetricClosure ('edge' x y)         == 'edges1' [(x,y), (y,x)]
-- symmetricClosure x                  == 'overlay' x ('transpose' x)
-- symmetricClosure . symmetricClosure == symmetricClosure
-- @
symmetricClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
symmetricClosure = coerce AM.symmetricClosure

-- | Compute the /transitive closure/ of a graph.
-- Complexity: /O(n * m * log(n)^2)/ time.
--
-- @
-- transitiveClosure ('vertex' x)          == 'vertex' x
-- transitiveClosure ('edge' x y)          == 'edge' x y
-- transitiveClosure ('path1' $ 'Data.List.NonEmpty.nub' xs)    == 'clique1' ('Data.List.NonEmpty.nub' xs)
-- transitiveClosure . transitiveClosure == transitiveClosure
-- @
transitiveClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure = coerce AM.transitiveClosure

-- TODO: Add tests.
-- | Check that the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices, and the graph is non-empty. It should be
-- impossible to create an inconsistent adjacency map, and we use this function
-- in testing.
--
-- @
-- consistent ('vertex' x)    == True
-- consistent ('overlay' x y) == True
-- consistent ('connect' x y) == True
-- consistent ('edge' x y)    == True
-- consistent ('edges' xs)    == True
-- consistent ('stars' xs)    == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (NAM x) = AM.consistent x && not (AM.isEmpty x)
