module Algebra.Graph.Acyclic.AdjacencyMap (
  -- * Data types and type aliases
  PartialOrder,

  -- * Data structure
  AdjacencyMap,

  -- * Internal Checks
  consistent,

  -- * Graph properties
  edgeList, vertexList,

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
import qualified Algebra.Graph.AdjacencyMap.Internal as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Graph.Typed as Typed
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
  } deriving (Show, Eq)

-- | Check if the internal graph representation is consistent,
-- i.e. that all edges refer to existing vertices and the graph
-- is acyclic. It should be impossible to create an inconsistent 
-- adjacency map.
--
-- @
-- consistent 'empty'         == True
-- consistent (1 + 2)         == True
-- consistent (1 * 2 + 2 * 3) == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (AAM m) = AM.consistent m && AM.isAcyclic m

-- | Construct the /empty acyclic graph/.
-- Complexity: /O(1)/ time and memory.
empty :: AdjacencyMap a
empty = AAM AM.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
vertex :: a -> AdjacencyMap a
vertex x = AAM $ AM.vertex x

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
vertices xs = AAM $ AM.vertices xs

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
-- transitiveClosure 'empty'             == 'empty'
-- transitiveClosure ('vertex' x)        == 'vertex' x
-- transitiveClosure (1 * 2 + 2 * 3)     == 1 * 2 + 2 * 3 + 1 * 3
-- transitiveClosure . transitiveClosure == transitiveClosure
-- @
transitiveClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure (AAM x) = AAM (AM.transitiveClosure x)

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
scc = AAM . AM.scc

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
box (AAM x) (AAM y) = AAM $ AM.overlay (fO $ xy y x) (fO $ yx x y)
  where
    xy q = AM.gmap (\x -> AM.gmap (x, ) q)
    yx p = AM.gmap (\y -> AM.gmap (, y) p)
    fO =
      Map.foldrWithKey (\k -> AM.overlay . Set.foldr AM.overlay k) AM.empty .
      AM.adjacencyMap

-- | This is a signature for a 'Strict Partial Order'.
-- A strict partial order is a binary relation 'R' that has three
-- axioms, namely, irreflexive, transitive and asymmetric.
--   > a 'R' a == False                    (Irreflexive)
--   > a 'R' b and b 'R' c => a 'R' c      (Transitive)
--   > a 'R' b == True => b 'R' a == False (Asymmetric)
-- Some examples of a Strict Partial Order are (<) and (>).
type PartialOrder a = a -> a -> Bool

-- | Constructs an acyclic graph from any graph based on
-- a strict partial order to produce an acyclic graph.
-- The partial order defines the valid set of edges.
-- For example, if the partial order is (<) then for any two
-- vertices x and y (x > y), the only possible edge is (y, x).
-- This will guarantee the production of an acyclic graph as no
-- back edges are nor possible.
--
-- For example,
-- 'fromGraph (<) (1 * 2 + 2 * 1) == 1 * 2' because
-- '1 < 2 == True' and hence the edge is allowed.
-- '2 < 1 == False' and hence the edge is filtered out.
--
-- @
-- fromGraph (<) (2 * 1)         == 'empty'
-- fromGraph (<) (1 * 2)         == 1 * 2
-- fromGraph (<) (1 * 2 + 2 * 1) == 1 * 2
-- @
fromGraph :: Ord a => PartialOrder a -> Graph a -> AdjacencyMap a
fromGraph o = AAM . aMF . foldg AM.empty AM.vertex AM.overlay AM.connect
  where
    aMF = AM.AM . Map.mapWithKey (\k -> Set.filter (k `o`)) . AM.adjacencyMap

-- | __Note:__ this does not satisfy the usual ring laws; see 
-- 'AdjacencyMap' for more details.
instance (Ord a, Num a) => Num (AdjacencyMap a) where
  fromInteger = AAM . fromInteger
  (AAM (AM.AM x)) + (AAM (AM.AM y)) =
    AAM . AM.AM $
    Map.unionWithKey (\k p q -> Set.filter (> k) $ Set.union p q) x y
  x'@(AAM (AM.AM x)) * y'@(AAM (AM.AM y)) =
    x' + y' +
    (AAM . AM.AM $
     Map.fromSet (\k -> Set.filter (> k) . Map.keysSet $ y) (Map.keysSet x))
  signum = const empty
  abs = id
  negate = id

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'      == []
-- edgeList ('vertex' x) == []
-- edgeList (1 * 2)      == [(1,2)]
-- edgeList (2 * 1)      == []
-- @
edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = AM.edgeList . aam

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: AdjacencyMap a -> [a]
vertexList = AM.vertexList . aam
