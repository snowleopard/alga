module Algebra.Graph.Acyclic.AdjacencyMap (
  -- * Data types and type aliases
  PartialOrder,

  -- * Data structure
  AdjacencyMap,

  -- * Internal Checks
  consistent,

  -- * Graph properties
  isEmpty, edgeList, vertexList, edgeSet, vertexSet,
  vertexCount, edgeCount, adjacencyList, hasEdge, hasVertex,

  -- * Graph transformation
  removeVertex, removeEdge, transpose, induce,

  -- * Basic graph construction primitives
  empty, vertex, overlayD, connectD, vertices,

  -- * Additional functions on acyclic graphs
  box, topSort, transitiveClosure,

  -- * Acyclic graph construction methods
  scc, fromGraph
  ) where

import Algebra.Graph (Graph, foldg)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Graph.Typed as Typed
import Data.Set (Set)
import Data.Coerce (coerce)

{-| The 'AdjacencyMap' data type represents an acyclic graph by
wrapping around 'Algebra.Graph.AdjacencyMap.AdjacencyMap'. The
idea is that the user of the library should not be given a way
to produce a cyclic graph.

We define a 'Num' instance as a convenient notation for working
with graphs:

  > edgeList   0           == []
  > vertexList 0           == [0]
  > edgeList   (1 + 2)     == []
  > vertexList (1 + 2)     == [1,2]
  > edgeList   (1 * 2)     == [(1,2)]
  > vertexList (1 * 2)     == [1,2]
  > edgeList   (1 + 2 * 3) == [(2,3)]
  > vertexList (1 + 2 * 3) == [1,2,3]
  > edgeList   (1 * 2 + 3) == [(1,2)]
  > vertexList (1 * 2 + 3) == [1,2,3]

The default partial order for the 'Num' instance to restrict
edges is (\<), ie. Edges can only be formed from vertex
numbered /a/ to a vertex numbered /b/ if and only if 
/a/ \< /b/:

  > edgeList (1 * 2) == [(1,2)]
  > edgeList (2 * 1) == []
  > (2 * 1) == (2 + 1)

__Note:__ the 'Num' instance does not satisfy several "customary laws"
of 'Num', which dictate that 'fromInteger' @0@ and 'fromInteger' @1@
should act as additive and multiplicative identities, and 'negate' as
additive inverse. Nevertheless, overloading 'fromInteger', '+' and '*'
is very convenient when working with algebraic graphs; we hope that
in future Haskell's Prelude will provide a more fine-grained class
hierarchy for algebraic structures, which we would be able to utilise
without violating any laws.
-}
newtype AdjacencyMap a = AAM
  { aam :: AM.AdjacencyMap a
  } deriving (Show, Eq, Ord)

-- | Check if the internal graph representation is consistent,
-- i.e. that all edges refer to existing vertices and the graph
-- is acyclic. It should be impossible to create an inconsistent 
-- adjacency map.
--
-- @
-- consistent 'empty'           == True
-- consistent (1 + 2)         == True
-- consistent (1 * 2 + 2 * 3) == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (AAM m) = AM.consistent m && AM.isAcyclic m

-- | Construct the /empty acyclic graph/.
-- Complexity: /O(1)/ time and memory.
empty :: AdjacencyMap a
empty = coerce AM.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
vertex :: a -> AdjacencyMap a
vertex = coerce AM.vertex

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is
-- the length of the given list.
--
-- @
-- vertices []        == 'empty'
-- vertices [x]       == 'vertex' x
-- vertices [1, 2, 3] == 1 + 2 + 3
-- @
vertices :: (Ord a) => [a] -> AdjacencyMap a
vertices = coerce AM.vertices

-- | Perform a disjoint overlay of two different acyclic graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edgeList (overlayD empty empty)             == []
-- edgeList (overlayD (1 * 2 + 1 * 3) (1 * 2)) == [(Left 1,Left 2) 
--                                                ,(Left 1,Left 3)
--                                                ,(Right 1,Right 2)]
-- @
overlayD ::
     (Ord a, Ord b)
  => AdjacencyMap a
  -> AdjacencyMap b
  -> AdjacencyMap (Either a b)
overlayD (AAM a) (AAM b) = AAM (AM.overlay (AM.gmap Left a) (AM.gmap Right b))

-- | Perform a disjoint connect of two different acyclic graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edgeList (connectD empty empty)     == []
-- edgeList (connectD (1 + 2) (1 + 2)) == [(Left 1,Right 1) 
--                                        ,(Left 1,Right 2)
--                                        ,(Left 2,Right 1)
--                                        ,(Left 2,Right 2)]
-- edgeList (connectD (1 * 2) (1 * 2)) == [(Left 1,Left 2) 
--                                        ,(Left 1,Right 1)
--                                        ,(Left 1,Right 2) 
--                                        ,(Left 2,Right 1) 
--                                        ,(Left 2,Right 2) 
--                                        ,(Right 1,Right 2)]
-- @
connectD ::
     (Ord a, Ord b)
  => AdjacencyMap a
  -> AdjacencyMap b
  -> AdjacencyMap (Either a b)
connectD (AAM a) (AAM b) = AAM (AM.connect (AM.gmap Left a) (AM.gmap Right b))

-- | Compute the /transitive closure/ of a graph.
-- Complexity: /O(n * m * log(n)^2)/ time.
--
-- @
-- transitiveClosure 'empty'               == 'empty'
-- transitiveClosure ('vertex' x)          == 'vertex' x
-- transitiveClosure (1 * 2 + 2 * 3)     == 1 * 2 + 2 * 3 + 1 * 3
-- transitiveClosure . transitiveClosure == transitiveClosure
-- @
transitiveClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure = coerce AM.transitiveClosure

-- | Compute the /condensation/ of a graph, where each vertex
-- corresponds to a /strongly-connected component/ of the original
-- graph. Note that component graphs are non-empty, and are therefore
-- of type "Algebra.Graph.NonEmpty.AdjacencyMap".
--
-- @
-- scc 'empty'      == 'empty'
-- scc ('vertex' x) == 'vertex' (NonEmpty.'NonEmpty.vertex' x)
-- @
scc :: (Ord a) => AM.AdjacencyMap a -> AdjacencyMap (NonEmpty.AdjacencyMap a)
scc = coerce AM.scc

-- | Compute the /topological sort/ of a graph.
-- 
-- @
-- topSort (1)             == [1]
-- topSort (1 * 2 + 3 * 1) == [3,1,2]
-- @
topSort :: (Ord a) => AdjacencyMap a -> [a]
topSort (AAM am) = Typed.topSort (Typed.fromAdjacencyMap am)

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O(s1 * s2)/ time, memory and size, where /s1/ and /s2/
-- are the sizes of the given graphs.
--
-- @
-- edgeList   (box (1 * 2) (3 * 4)) == [((1,3),(1,4))
--                                     ,((1,3),(2,3))
--                                     ,((1,4),(2,4))
--                                     ,((2,3),(2,4))]
-- edgeList   (box (1 + 2) (3 + 4)) == []
-- vertexList (box (1 + 2) (3 + 4)) == [(1,3),(1,4),(2,3),(2,4)]
-- @
box :: (Ord a, Ord b) => AdjacencyMap a -> AdjacencyMap b -> AdjacencyMap (a, b)
box = coerce AM.box

-- | Remove a vertex from a given acyclic graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex 1 (1 * 2)          == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
removeVertex = coerce AM.removeVertex

-- | Remove an edge from a given acyclic graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge 1 2 (1 * 2)          == (1 + 2)
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 2 (1 * 2 + 3 * 4)  == 1 + 2 + 3 * 4
-- @
removeEdge :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
removeEdge = coerce AM.removeEdge

-- | This is a signature for a __Strict Partial Order__.
-- A strict partial order is a binary relation __/R/__ that has three
-- axioms, namely, irreflexive, transitive and asymmetric.
--
--   > a 'R' a == False               (Irreflexive)
--   > a 'R' b and b 'R' c => a 'R' c (Transitive)
-- Some examples of a Strict Partial Order are 
-- __\<__ and __\>__.
type PartialOrder a = a -> a -> Bool

-- | Constructs an acyclic graph from any graph based on
-- a strict partial order to produce an acyclic graph.
-- The partial order defines the valid set of edges.
-- 
-- If the partial order is \< then for any two
-- vertices x and y (x \> y), the only possible edge is (y, x).
-- This will guarantee the production of an acyclic graph since
-- no back edges are possible.
--
-- For example,
-- /fromGraph (\<) (1 \* 2 + 2 \* 1) == 1 \* 2/ because
-- /1 \< 2 == True/ and hence the edge is allowed.
-- /2 \< 1 == False/ and hence the edge is filtered out.
--
-- @
-- fromGraph (<) (2 * 1)         == 1 + 2
-- fromGraph (<) (1 * 2)         == 1 * 2
-- fromGraph (<) (1 * 2 + 2 * 1) == 1 * 2
-- @
fromGraph :: Ord a => PartialOrder a -> Graph a -> AdjacencyMap a
fromGraph o =
  AAM . induceEAM (uncurry o) . foldg AM.empty AM.vertex AM.overlay AM.connect

-- | __Note:__ this does not satisfy the usual ring laws; see 
-- 'AdjacencyMap' for more details.
instance (Ord a, Num a) => Num (AdjacencyMap a) where
  fromInteger = AAM . fromInteger
  (AAM x) + (AAM y) = AAM $ induceEAM (uncurry (<)) (x + y)
  (AAM x) * (AAM y) = AAM $ induceEAM (uncurry (<)) (x * y)
  signum = const empty
  abs = id
  negate = id

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'      == []
-- edgeList ('vertex' x) == []
-- edgeList (1 * 2)    == [(1,2)]
-- edgeList (2 * 1)    == []
-- @
edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = coerce AM.edgeList

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: AdjacencyMap a -> [a]
vertexList = coerce AM.vertexList

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
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
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount (1 * 2)    == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyMap a -> Int
edgeCount = coerce AM.edgeCount

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: AdjacencyMap a -> Set a
vertexSet = coerce AM.vertexSet

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet (1 * 2)    == Set.'Set.singleton' (1,2)
-- @
edgeSet :: Eq a => AdjacencyMap a -> Set (a, a)
edgeSet = coerce AM.edgeSet

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'      == []
-- adjacencyList ('vertex' x) == [(x, [])]
-- adjacencyList (1 * 2)    == [(1, [2]), (2, [])]
-- @
adjacencyList :: AdjacencyMap a -> [(a, [a])]
adjacencyList = coerce AM.adjacencyList

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' 1 2 $ 1 * 2)    == False
-- @
isEmpty :: AdjacencyMap a -> Bool
isEmpty = coerce AM.isEmpty

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Ord a => a -> AdjacencyMap a -> Bool
hasVertex = coerce AM.hasVertex

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge 1 2 (1 * 2)          == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap a -> Bool
hasEdge = coerce AM.hasEdge

-- | Transpose a given acyclic graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose (1 * 2)     == (2 * 1)
-- transpose . transpose == id
-- 'edgeList' . transpose  == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: Ord a => AdjacencyMap a -> AdjacencyMap a
transpose = coerce AM.transpose

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce ('const' True ) x == x
-- induce ('const' False) x == 'empty'
-- induce (/= x)           == 'removeVertex' x
-- induce p . induce q    == induce (\\x -> p x && q x)
-- @
induce :: (a -> Bool) -> AdjacencyMap a -> AdjacencyMap a
induce = coerce AM.induce

-- Helper function, not to be exported.
-- Induce a subgraph from AM.AdjacencyList removing edges not
-- following the given predicate.
induceEAM ::
     (Ord a) => ((a, a) -> Bool) -> AM.AdjacencyMap a -> AM.AdjacencyMap a
induceEAM p m = es m `AM.overlay` vs m
  where
    es = AM.edges . filter p . AM.edgeList
    vs = AM.vertices . AM.vertexList
