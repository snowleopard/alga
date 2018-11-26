-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.NonEmpty.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
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
    AdjacencyMap, toNonEmpty,

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
    induce1,

    -- * Graph closure
    closure, reflexiveClosure, symmetricClosure, transitiveClosure
    ) where

import Prelude hiding (reverse)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList, reverse)
import Data.Maybe
import Data.Set (Set)
import Data.Tree

import Algebra.Graph.NonEmpty.AdjacencyMap.Internal

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Set                   as Set

-- Lift a function to non-empty adjacency maps
via :: (AM.AdjacencyMap a -> AM.AdjacencyMap b)
    ->     AdjacencyMap a ->    AdjacencyMap b
via f = NAM . f . am

-- Lift a two-argument function to non-empty adjacency maps
via2 :: (AM.AdjacencyMap a -> AM.AdjacencyMap b -> AM.AdjacencyMap c)
     ->     AdjacencyMap a ->    AdjacencyMap b ->    AdjacencyMap c
via2 f (NAM x) (NAM y) = NAM (f x y)

-- Lift a list function to non-empty adjacency maps
viaL :: (         [AM.AdjacencyMap a] -> AM.AdjacencyMap b)
     ->  NonEmpty (   AdjacencyMap a) ->    AdjacencyMap b
viaL f = NAM . f . fmap am . toList

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
-- toNonEmpty 'AM.empty'              == Nothing
-- toNonEmpty ('Algebra.Graph.ToGraph.toAdjacencyMap' x) == Just (x :: 'AdjacencyMap' a)
-- @
toNonEmpty :: AM.AdjacencyMap a -> Maybe (AdjacencyMap a)
toNonEmpty x | AM.isEmpty x = Nothing
             | otherwise    = Just (NAM x)

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'AdjacencyMap.hasVertex' x (vertex x) == True
-- 'AdjacencyMap.vertexCount' (vertex x) == 1
-- 'AdjacencyMap.edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap a
vertex = NAM . AM.vertex
{-# NOINLINE [1] vertex #-}

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
overlay = via2 AM.overlay
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
connect = via2 AM.connect
{-# NOINLINE [1] connect #-}

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Ord a => a -> a -> AdjacencyMap a
edge x y = NAM (AM.edge x y)

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
vertices1 = NAM . AM.vertices . toList
{-# NOINLINE [1] vertices1 #-}

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges1 [(x,y)]     == 'edge' x y
-- 'edgeCount' . edges1 == 'Data.List.NonEmpty.length' . 'Data.List.NonEmpty.nub'
-- @
edges1 :: Ord a => NonEmpty (a, a) -> AdjacencyMap a
edges1 = NAM . AM.edges . toList

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays1 [x]   == x
-- overlays1 [x,y] == 'overlay' x y
-- @
overlays1 :: Ord a => NonEmpty (AdjacencyMap a) -> AdjacencyMap a
overlays1 = viaL AM.overlays
{-# NOINLINE overlays1 #-}

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects1 [x]   == x
-- connects1 [x,y] == 'connect' x y
-- @
connects1 :: Ord a => NonEmpty (AdjacencyMap a) -> AdjacencyMap a
connects1 = viaL AM.connects
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
isSubgraphOf (NAM x) (NAM y) = AM.isSubgraphOf x y

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x ('vertex' x) == True
-- hasVertex 1 ('vertex' 2) == False
-- @
hasVertex :: Ord a => a -> AdjacencyMap a -> Bool
hasVertex x = AM.hasVertex x . am

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
hasEdge x y = AM.hasEdge x y . am

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: AdjacencyMap a -> Int
vertexCount = AM.vertexCount . am

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyMap a -> Int
edgeCount = AM.edgeCount . am

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList1 ('vertex' x)  == [x]
-- vertexList1 . 'vertices1' == 'Data.List.NonEmpty.nub' . 'Data.List.NonEmpty.sort'
-- @
vertexList1 :: AdjacencyMap a -> NonEmpty a
vertexList1 = unsafeNonEmpty . AM.vertexList . am

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
edgeList = AM.edgeList . am

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet . 'vertex'    == Set.'Set.singleton'
-- vertexSet . 'vertices1' == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- vertexSet . 'clique1'   == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertexSet :: AdjacencyMap a -> Set a
vertexSet = AM.vertexSet . am

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Ord a => AdjacencyMap a -> Set (a, a)
edgeSet = Set.fromAscList . edgeList

-- | The /preset/ of an element @x@ is the set of its /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preSet x ('vertex' x) == Set.'Set.empty'
-- preSet 1 ('edge' 1 2) == Set.'Set.empty'
-- preSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preSet :: Ord a => a -> AdjacencyMap a -> Set.Set a
preSet x = AM.preSet x . am

-- | The /postset/ of a vertex is the set of its /direct successors/.
-- Complexity: /O(log(n))/ time and /O(1)/ memory.
--
-- @
-- postSet x ('vertex' x) == Set.'Set.empty'
-- postSet x ('edge' x y) == Set.'Set.fromList' [y]
-- postSet 2 ('edge' 1 2) == Set.'Set.empty'
-- @
postSet :: Ord a => a -> AdjacencyMap a -> Set a
postSet x = AM.postSet x . am

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path1 [x]       == 'vertex' x
-- path1 [x,y]     == 'edge' x y
-- path1 . 'Data.List.NonEmpty.reverse' == 'transpose' . path1
-- @
path1 :: Ord a => NonEmpty a -> AdjacencyMap a
path1 = NAM . AM.path . toList

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit1 [x]       == 'edge' x x
-- circuit1 [x,y]     == 'edges1' [(x,y), (y,x)]
-- circuit1 . 'Data.List.NonEmpty.reverse' == 'transpose' . circuit1
-- @
circuit1 :: Ord a => NonEmpty a -> AdjacencyMap a
circuit1 = NAM . AM.circuit . toList

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
clique1 = NAM . AM.clique . toList
{-# NOINLINE [1] clique1 #-}

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique1 [x1,x2] [y1,y2] == 'edges1' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique1 xs      ys      == 'connect' ('vertices1' xs) ('vertices1' ys)
-- @
biclique1 :: Ord a => NonEmpty a -> NonEmpty a -> AdjacencyMap a
biclique1 xs ys = NAM $ AM.biclique (toList xs) (toList ys)

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
star x = NAM . AM.star x
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
stars1 = NAM . AM.stars . toList

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
tree = NAM . AM.tree

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
removeVertex1 x = toNonEmpty . AM.removeVertex x . am

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
removeEdge x y = via (AM.removeEdge x y)

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
replaceVertex u v = via (AM.replaceVertex u v)

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: Ord a => (a -> Bool) -> a -> AdjacencyMap a -> AdjacencyMap a
mergeVertices p v = via (AM.mergeVertices p v)

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
transpose = via AM.transpose
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
gmap f = via (AM.gmap f)

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce1 ('const' True ) x == Just x
-- induce1 ('const' False) x == Nothing
-- induce1 (/= x)          == 'removeVertex1' x
-- induce1 p 'Control.Monad.>=>' induce1 q == induce1 (\\x -> p x && q x)
-- @
induce1 :: (a -> Bool) -> AdjacencyMap a -> Maybe (AdjacencyMap a)
induce1 p = toNonEmpty . AM.induce p . am

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
closure = via (AM.closure)

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
reflexiveClosure = via AM.reflexiveClosure

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
symmetricClosure = via AM.symmetricClosure

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
transitiveClosure = via AM.transitiveClosure
