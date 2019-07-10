-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Acyclic.Labelled.AdjacencyMap
-- License    : MIT (see the file LICENSE)
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'AdjacencyMap' data type for edge-labelled acyclic graphs, as
-- well as associated operations and algorithms.
-----------------------------------------------------------------------------
module Algebra.Graph.Acyclic.Labelled.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, fromAcyclic,

    -- * Basic graph construction primitives
    empty, vertex, vertices,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, edgeLabel, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, edgeSet,

    -- * Graph transformation
    removeVertex, removeEdge, transpose, emap, induce, induceJust,

    -- * Relational operations
    transitiveClosure,

    -- * Miscellaneous
    -- consistent
    ) where

import Data.Set (Set)
import Algebra.Graph.Label
import Data.Function (on)

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM

-- | Edge-labelled graphs, where the type variable @e@ stands for edge labels.
-- For example, 'AdjacencyMap' @Bool@ @a@ is isomorphic to unlabelled acyclic graphs
-- defined in the top-level module "Algebra.Graph.AdjacencyMap", where @False@
-- and @True@ denote the lack of and the existence of an unlabelled edge,
-- respectively.
newtype AdjacencyMap e a = AAM {
    -- | The /adjacency map/ of an edge-labelled graph: each vertex is
    -- associated with a map from its direct successors to the corresponding
    -- edge labels.
    fromAcyclic :: AM.AdjacencyMap e a } deriving (Eq, Ord)

-- TODO: Improve the Show instance.
instance (Ord a, Show a, Ord e, Show e) => Show (AdjacencyMap e a) where
  show x = "fromMaybe empty . toAcyclic $ " ++ show (fromAcyclic x)

-- | __Note:__ this does not satisfy the usual ring laws; see 'AdjacencyMap'
-- for more details.
instance (Eq e, Dioid e, Ord a, Num a) => Num (AdjacencyMap e a) where
  fromInteger = vertex . fromInteger
  (AAM x) + (AAM y) = AAM $ induceEAM (uncurry (<)) (x + y)
  (AAM x) * (AAM y) = AAM $ induceEAM (uncurry (<)) (x * y)
  signum = const empty
  abs = id
  negate = id

-- TODO: Add additional/change examples.

-- TODO: Add the consistent function.

-- TODO: Make 'skeleton :: Ord a => AdjacencyMap e a -> AAM.AdjacencyMap a'

-- TODO: Add 'fromGraph'

-- TODO: Add 'toAcyclic'

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: AdjacencyMap e a
empty = AAM AM.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap e a
vertex = AAM . AM.vertex

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Ord a => [a] -> AdjacencyMap e a
vertices = AAM . AM.vertices

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- isSubgraphOf 'empty'      x     ==  True
-- isSubgraphOf ('vertex' x) 'empty' ==  False
-- isSubgraphOf x y              ==> x <= y
-- @
isSubgraphOf :: (Eq e, Monoid e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a -> Bool
isSubgraphOf = AM.isSubgraphOf `on` fromAcyclic

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                         == True
-- isEmpty ('vertex' x)                    == False
-- isEmpty ('removeVertex' x $ 'vertex' x)   == True
-- @
isEmpty :: AdjacencyMap e a -> Bool
isEmpty = AM.isEmpty . fromAcyclic

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Ord a => a -> AdjacencyMap e a -> Bool
hasVertex x = AM.hasVertex x . fromAcyclic

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'not' . 'null' . 'filter' (\\(_,ex,ey) -> ex == x && ey == y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap e a -> Bool
hasEdge x y = AM.hasEdge x y . fromAcyclic

-- | Extract the label of a specified edge in a graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- edgeLabel x y 'empty'         == 'zero'
-- edgeLabel x y ('vertex' z)    == 'zero'
-- @
edgeLabel :: (Monoid e, Ord a) => a -> a -> AdjacencyMap e a -> e
edgeLabel x y = AM.edgeLabel x y . fromAcyclic

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: AdjacencyMap e a -> Int
vertexCount = AM.vertexCount . fromAcyclic

-- | The number of (non-'zero') edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'        == 0
-- edgeCount ('vertex' x)   == 0
-- edgeCount              == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyMap e a -> Int
edgeCount = AM.edgeCount . fromAcyclic

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: AdjacencyMap e a -> [a]
vertexList = AM.vertexList . fromAcyclic

-- | The list of edges of a graph, sorted lexicographically with respect to
-- pairs of connected vertices (i.e. edge-labels are ignored when sorting).
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'        == []
-- edgeList ('vertex' x)   == []
-- @
edgeList :: AdjacencyMap e a -> [(e, a, a)]
edgeList = AM.edgeList . fromAcyclic

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: AdjacencyMap e a -> Set a
vertexSet = AM.vertexSet . fromAcyclic

-- | The set of edges of a given graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'        == Set.'Set.empty'
-- edgeSet ('vertex' x)   == Set.'Set.empty'
-- @
edgeSet :: (Eq a, Eq e) => AdjacencyMap e a -> Set (e, a, a)
edgeSet = AM.edgeSet . fromAcyclic

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> AdjacencyMap e a -> AdjacencyMap e a
removeVertex x = AAM . AM.removeVertex x . fromAcyclic

-- | Remove an edge from a given graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge x y ('edge' e x y)     == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- @
removeEdge :: Ord a => a -> a -> AdjacencyMap e a -> AdjacencyMap e a
removeEdge x y = AAM . AM.removeEdge x y . fromAcyclic

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose 'empty'        == 'empty'
-- transpose ('vertex' x)   == 'vertex' x
-- transpose . transpose  == id
-- @
transpose :: (Monoid e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a
transpose = AAM . AM.transpose . fromAcyclic

-- | Transform a graph by applying a function @h@ to each of its edge labels.
-- Complexity: /O((n + m) * log(n))/ time.
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
-- emap 'id'                == 'id'
-- emap g . emap h        == emap (g . h)
-- @
emap :: (Eq f, Monoid f) => (e -> f) -> AdjacencyMap e a -> AdjacencyMap f a
emap h = AAM . AM.emap h . fromAcyclic

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> AdjacencyMap e a -> AdjacencyMap e a
induce f = AAM . AM.induce f . fromAcyclic

-- | Construct the /induced subgraph/ of a given graph by removing the vertices
-- that are 'Nothing'.
-- Complexity: /O(n + m)/ time.
--
-- @
-- induceJust ('vertex' 'Nothing')                               == 'empty'
-- @
induceJust :: Ord a => AdjacencyMap e (Maybe a) -> AdjacencyMap e a
induceJust = AAM . AM.induceJust . fromAcyclic

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
transitiveClosure :: (Eq e, Ord a, StarSemiring e) => AdjacencyMap e a -> AdjacencyMap e a
transitiveClosure = AAM  . AM.transitiveClosure . fromAcyclic

-- Helper function, not to be exported.
-- Induce a subgraph from AM.AdjacencyList removing edges not
-- following the given predicate.
induceEAM ::
     (Eq e, Monoid e, Ord a) => ((a, a) -> Bool) -> AM.AdjacencyMap e a -> AM.AdjacencyMap e a
induceEAM p m = es m `AM.overlay` vs m
  where
    w (_, a, b) = (a, b)
    es = AM.edges . filter (p . w) . AM.edgeList
    vs = AM.vertices . AM.vertexList
