-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Acyclic.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for
-- the motivation behind the library, the underlying theory, and implementation
-- details.
--
-- This module defines the 'AdjacencyMap' data type and for acyclic graphs, as
-- well as associated operations and algorithms. To avoid name clashes with
-- "Algebra.Graph.AdjacencyMap", this module can be imported qualified:
--
-- @
-- import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
-- @
-----------------------------------------------------------------------------
module Algebra.Graph.Acyclic.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, fromAcyclic,

    -- * Basic graph construction primitives
    empty, vertex, vertices, union, join,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexSet, edgeSet, preSet, postSet,

    -- * Graph transformation
    removeVertex, removeEdge, transpose, induce, induceJust,

    -- * Graph composition
    box,

    -- * Relational operations
    transitiveClosure,

    -- * Algorithms
    topSort, scc,

    -- * Conversion to acyclic graphs
    toAcyclic, toAcyclicOrd, shrink,

    -- * Miscellaneous
    consistent
    ) where

import Data.Set (Set)
import Data.Coerce (coerce)

import qualified Algebra.Graph.AdjacencyMap           as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as NAM
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set

{-| The 'AdjacencyMap' data type represents an acyclic graph by a map of
vertices to their adjacency sets. Although the internal representation allows
for cycles, the methods provided by this module cannot be used to construct a
graph with cycles.

The 'Show' instance is defined using basic graph construction primitives where
possible, falling back to 'toAcyclic' and "Algebra.Graph.AdjacencyMap"
otherwise:

@
show empty                == "empty"
show (shrink 1)           == "vertex 1"
show (shrink $ 1 + 2)     == "vertices [1,2]"
show (shrink $ 1 * 2)     == "(fromJust . toAcyclic) (edge 1 2)"
show (shrink $ 1 * 2 * 3) == "(fromJust . toAcyclic) (edges [(1,2),(1,3),(2,3)])"
show (shrink $ 1 * 2 + 3) == "(fromJust . toAcyclic) (overlay (vertex 3) (edge 1 2))"
@

The total order on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Note that the resulting order refines the 'isSubgraphOf' relation:

@'isSubgraphOf' x y ==> x <= y@
-}

-- TODO: Improve the Show instance.
newtype AdjacencyMap a = AAM {
    -- | Extract the underlying acyclic "Algebra.Graph.AdjacencyMap".
    -- Complexity: /O(1)/ time and memory.
    --
    -- @
    -- fromAcyclic 'empty'                == 'AM.empty'
    -- fromAcyclic . 'vertex'             == 'AM.vertex'
    -- fromAcyclic (shrink $ 1 * 3 + 2) == 1 * 3 + 2
    -- 'AM.vertexCount' . fromAcyclic        == 'vertexCount'
    -- 'AM.edgeCount'   . fromAcyclic        == 'edgeCount'
    -- 'AM.isAcyclic'   . fromAcyclic        == 'const' True
    -- @
    fromAcyclic :: AM.AdjacencyMap a
    } deriving (Eq, Ord)

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    showsPrec p aam@(AAM am)
        | null vs    = showString "empty"
        | null es    = showParen (p > 10) $ vshow vs
        | otherwise  = showParen (p > 10) $ showString "(fromJust . toAcyclic) ("
                     . shows am . showString ")"
      where
        vs             = vertexList aam
        es             = edgeList aam
        vshow [x]      = showString "vertex "   . showsPrec 11 x
        vshow xs       = showString "vertices " . showsPrec 11 xs

-- | Construct the /empty graph/.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: AdjacencyMap a
empty = coerce AM.empty

-- | Construct the graph comprising /a single isolated vertex/.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex y) == (x == y)
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap a
vertex = coerce AM.vertex

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
vertices :: Ord a => [a] -> AdjacencyMap a
vertices = coerce AM.vertices

-- | Construct the disjoint /union/ of two graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'vertexSet' (union x y) == Set.'Set.unions' [ Set.'Set.map' 'Left'  ('vertexSet' x)
--                                     , Set.'Set.map' 'Right' ('vertexSet' y) ]
--
-- 'edgeSet'   (union x y) == Set.'Set.unions' [ Set.'Set.map' ('Data.Bifunctor.bimap' 'Left'  'Left' ) ('edgeSet' x)
--                                     , Set.'Set.map' ('Data.Bifunctor.bimap' 'Right' 'Right') ('edgeSet' y) ]
-- @
union :: (Ord a, Ord b) => AdjacencyMap a -> AdjacencyMap b -> AdjacencyMap (Either a b)
union (AAM x) (AAM y) = AAM $ AM.overlay (AM.gmap Left x) (AM.gmap Right y)

-- | Construct the /join/ of two graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'vertexSet' (join x y) == Set.'Set.unions' [ Set.'Set.map' 'Left'  ('vertexSet' x)
--                                    , Set.'Set.map' 'Right' ('vertexSet' y) ]
--
-- 'edgeSet'   (join x y) == Set.'Set.unions' [ Set.'Set.map' ('Data.Bifunctor.bimap' 'Left'  'Left' ) ('edgeSet' x)
--                                    , Set.'Set.map' ('Data.Bifunctor.bimap' 'Right' 'Right') ('edgeSet' y)
--                                    , Set.'Set.map' ('Data.Bifunctor.bimap' 'Left'  'Right') (Set.'Set.cartesianProduct' ('vertexSet' x) ('vertexSet' y)) ]
-- @
join :: (Ord a, Ord b) => AdjacencyMap a -> AdjacencyMap b -> AdjacencyMap (Either a b)
join (AAM a) (AAM b) = AAM $ AM.connect (AM.gmap Left a) (AM.gmap Right b)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- isSubgraphOf 'empty'        x                     ==  True
-- isSubgraphOf ('vertex' x)   'empty'                 ==  False
-- isSubgraphOf ('induce' p x) x                     ==  True
-- isSubgraphOf x            ('transitiveClosure' x) ==  True
-- isSubgraphOf x y                                ==> x <= y
-- @
isSubgraphOf :: Ord a => AdjacencyMap a -> AdjacencyMap a -> Bool
isSubgraphOf = coerce AM.isSubgraphOf

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                             == True
-- isEmpty ('vertex' x)                        == False
-- isEmpty ('removeVertex' x $ 'vertex' x)       == True
-- isEmpty ('removeEdge' 1 2 $ shrink $ 1 * 2) == False
-- @
isEmpty :: AdjacencyMap a -> Bool
isEmpty = coerce AM.isEmpty

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' y)       == (x == y)
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
-- hasEdge 1 2 (shrink $ 1 * 2) == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap a -> Bool
hasEdge = coerce AM.hasEdge

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
-- edgeCount 'empty'            == 0
-- edgeCount ('vertex' x)       == 0
-- edgeCount (shrink $ 1 * 2) == 1
-- edgeCount                  == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyMap a -> Int
edgeCount = coerce AM.edgeCount

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

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'            == []
-- edgeList ('vertex' x)       == []
-- edgeList (shrink $ 2 * 1) == [(2,1)]
-- edgeList . 'transpose'      == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . edgeList
-- @
edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = coerce AM.edgeList

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and memory.
--
-- @
-- adjacencyList 'empty'            == []
-- adjacencyList ('vertex' x)       == [(x, [])]
-- adjacencyList (shrink $ 1 * 2) == [(1, [2]), (2, [])]
-- @
adjacencyList :: AdjacencyMap a -> [(a, [a])]
adjacencyList = coerce AM.adjacencyList

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
-- edgeSet 'empty'            == Set.'Set.empty'
-- edgeSet ('vertex' x)       == Set.'Set.empty'
-- edgeSet (shrink $ 1 * 2) == Set.'Set.singleton' (1,2)
-- @
edgeSet :: Eq a => AdjacencyMap a -> Set (a, a)
edgeSet = coerce AM.edgeSet

-- | The /preset/ of an element @x@ is the set of its /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preSet x 'empty'            == Set.'Set.empty'
-- preSet x ('vertex' x)       == Set.'Set.empty'
-- preSet 1 (shrink $ 1 * 2) == Set.'Set.empty'
-- preSet 2 (shrink $ 1 * 2) == Set.'Set.fromList' [1]
-- Set.'Set.member' x . preSet x   == 'const' False
-- @
preSet :: Ord a => a -> AdjacencyMap a -> Set a
preSet = coerce AM.preSet

-- | The /postset/ of a vertex is the set of its /direct successors/.
-- Complexity: /O(log(n))/ time and /O(1)/ memory.
--
-- @
-- postSet x 'empty'            == Set.'Set.empty'
-- postSet x ('vertex' x)       == Set.'Set.empty'
-- postSet 1 (shrink $ 1 * 2) == Set.'Set.fromList' [2]
-- postSet 2 (shrink $ 1 * 2) == Set.'Set.empty'
-- Set.'Set.member' x . postSet x   == 'const' False
-- @
postSet :: Ord a => a -> AdjacencyMap a -> Set a
postSet = coerce AM.postSet

-- | Remove a vertex from a given acyclic graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex 1 (shrink $ 1 * 2) == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
removeVertex = coerce AM.removeVertex

-- | Remove an edge from a given acyclic graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge 1 2 (shrink $ 1 * 2)     == 'vertices' [1,2]
-- removeEdge x y . removeEdge x y     == removeEdge x y
-- removeEdge x y . 'removeVertex' x     == 'removeVertex' x
-- removeEdge 1 2 (shrink $ 1 * 2 * 3) == shrink ((1 + 2) * 3)
-- @
removeEdge :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
removeEdge = coerce AM.removeEdge

-- | Transpose a given acyclic graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose . transpose == id
-- 'edgeList' . transpose  == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: Ord a => AdjacencyMap a -> AdjacencyMap a
transpose = coerce AM.transpose

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(n + m)/ time, assuming that the predicate takes constant time.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> AdjacencyMap a -> AdjacencyMap a
induce = coerce AM.induce

-- | Construct the /induced subgraph/ of a given graph by removing the vertices
-- that are 'Nothing'.
-- Complexity: /O(n + m)/ time.
--
-- @
-- induceJust ('vertex' 'Nothing') == 'empty'
-- induceJust . 'vertex' . 'Just'  == 'vertex'
-- @
induceJust :: Ord a => AdjacencyMap (Maybe a) -> AdjacencyMap a
induceJust = coerce AM.induceJust

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O((n + m) * log(n))/ time and O(n + m) memory.
--
-- @
-- 'edgeList' (box ('shrink' $ 1 * 2) ('shrink' $ 10 * 20)) == [ ((1,10), (1,20))
--                                                       , ((1,10), (2,10))
--                                                       , ((1,20), (2,20))
--                                                       , ((2,10), (2,20)) ]
-- @
--
-- Up to the isomorphism between the resulting vertex types, this operation
-- is /commutative/ and /associative/, has singleton graphs as /identities/ and
-- 'empty' as the /annihilating zero/. Below @~~@ stands for equality up to
-- the isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y               ~~ box y x
-- box x (box y z)       ~~ box (box x y) z
-- box x ('vertex' ())     ~~ x
-- box x 'empty'           ~~ 'empty'
-- 'transpose'   (box x y) == box ('transpose' x) ('transpose' y)
-- 'vertexCount' (box x y) == 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (box x y) <= 'vertexCount' x * 'edgeCount' y + 'edgeCount' x * 'vertexCount' y
-- @
box :: (Ord a, Ord b) => AdjacencyMap a -> AdjacencyMap b -> AdjacencyMap (a, b)
box = coerce AM.box

-- | Compute the /transitive closure/ of a graph.
-- Complexity: /O(n * m * log(n)^2)/ time.
--
-- @
-- transitiveClosure 'empty'                    == 'empty'
-- transitiveClosure ('vertex' x)               == 'vertex' x
-- transitiveClosure (shrink $ 1 * 2 + 2 * 3) == shrink (1 * 2 + 1 * 3 + 2 * 3)
-- transitiveClosure . transitiveClosure      == transitiveClosure
-- @
transitiveClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure = coerce AM.transitiveClosure

-- | Compute a /topological sort/ of an acyclic graph.
--
-- @
-- topSort 'empty'                          == []
-- topSort ('vertex' x)                     == [x]
-- topSort (shrink $ 1 * (2 + 4) + 3 * 4) == [1, 2, 3, 4]
-- topSort ('join' x y)                     == 'fmap' 'Left' (topSort x) ++ 'fmap' 'Right' (topSort y)
-- 'Right' . topSort                        == 'AM.topSort' . 'fromAcyclic'
-- @
topSort :: Ord a => AdjacencyMap a -> [a]
topSort g = case AM.topSort (coerce g) of
  Right vs -> vs
  Left _ -> error "Internal error: the acyclicity invariant is violated in topSort"

-- | Compute the acyclic /condensation/ of a graph, where each vertex
-- corresponds to a /strongly-connected component/ of the original graph. Note
-- that component graphs are non-empty, and are therefore of type
-- "Algebra.Graph.NonEmpty.AdjacencyMap".
--
-- @
--            scc 'AM.empty'               == 'empty'
--            scc ('AM.vertex' x)          == 'vertex' (NonEmpty.'NonEmpty.vertex' x)
--            scc ('AM.edge' 1 1)          == 'vertex' (NonEmpty.'NonEmpty.edge' 1 1)
-- 'edgeList' $ scc ('AM.edge' 1 2)          == [ (NonEmpty.'NonEmpty.vertex' 1       , NonEmpty.'NonEmpty.vertex' 2       ) ]
-- 'edgeList' $ scc (3 * 1 * 4 * 1 * 5) == [ (NonEmpty.'NonEmpty.vertex' 3       , NonEmpty.'NonEmpty.vertex' 5       )
--                                       , (NonEmpty.'NonEmpty.vertex' 3       , NonEmpty.'NonEmpty.clique1' [1,4,1])
--                                       , (NonEmpty.'NonEmpty.clique1' [1,4,1], NonEmpty.'NonEmpty.vertex' 5       ) ]
-- @
scc :: (Ord a) => AM.AdjacencyMap a -> AdjacencyMap (NAM.AdjacencyMap a)
scc = coerce AM.scc

-- | Construct an acyclic graph from a given adjacency map, or return 'Nothing'
-- if the input contains cycles.
--
-- @
-- toAcyclic ('AM.path'    [1,2,3]) == 'Just' (shrink $ 1 * 2 + 2 * 3)
-- toAcyclic ('AM.clique'  [3,2,1]) == 'Just' ('transpose' (shrink $ 1 * 2 * 3))
-- toAcyclic ('AM.circuit' [1,2,3]) == 'Nothing'
-- toAcyclic . 'fromAcyclic'     == 'Just'
-- @
toAcyclic :: Ord a => AM.AdjacencyMap a -> Maybe (AdjacencyMap a)
toAcyclic x = if AM.isAcyclic x then Just (AAM x) else Nothing

-- | Construct an acyclic graph from a given adjacency map, keeping only edges
-- @(x,y)@ where @x < y@ according to the supplied 'Ord' @a@ instance.
--
-- @
-- toAcyclicOrd 'empty'       == 'empty'
-- toAcyclicOrd . 'vertex'    == 'vertex'
-- toAcyclicOrd (1 + 2)     == shrink (1 + 2)
-- toAcyclicOrd (1 * 2)     == shrink (1 * 2)
-- toAcyclicOrd (2 * 1)     == shrink (1 + 2)
-- toAcyclicOrd (1 * 2 * 1) == shrink (1 * 2)
-- toAcyclicOrd (1 * 2 * 3) == shrink (1 * 2 * 3)
-- @
toAcyclicOrd :: Ord a => AM.AdjacencyMap a -> AdjacencyMap a
toAcyclicOrd = AAM . filterEdges (<)

-- TODO: Add time complexity
-- TODO: Change Arbitrary instance of Acyclic and Labelled Acyclic graph
-- | Construct an acyclic graph from a given adjacency map using 'scc'.
-- If the graph is acyclic, it is returned as is. If the graph is cyclic, then a
-- representative for every strongly connected component in its condensation
-- graph is chosen and these representatives are used to build an acyclic graph.
--
-- @
-- shrink . 'AM.vertex'      == 'vertex'
-- shrink . 'AM.vertices'    == 'vertices'
-- shrink . 'fromAcyclic' == 'id'
-- @
shrink :: Ord a => AM.AdjacencyMap a -> AdjacencyMap a
shrink = AAM . AM.gmap (NonEmpty.head . NAM.vertexList1) . AM.scc

-- TODO: Provide a faster equivalent in "Algebra.Graph.AdjacencyMap".
-- Keep only the edges that satisfy a given predicate.
filterEdges :: Ord a => (a -> a -> Bool) -> AM.AdjacencyMap a -> AM.AdjacencyMap a
filterEdges p m = AM.fromAdjacencySets
    [ (a, Set.filter (p a) bs) | (a, bs) <- Map.toList (AM.adjacencyMap m) ]

-- | Check if the internal representation of an acyclic graph is consistent,
-- i.e. that all edges refer to existing vertices and the graph is acyclic. It
-- should be impossible to create an inconsistent 'AdjacencyMap'.
--
-- @
-- consistent 'empty'                 == True
-- consistent ('vertex' x)            == True
-- consistent ('vertices' xs)         == True
-- consistent ('union' x y)           == True
-- consistent ('join' x y)            == True
-- consistent ('transpose' x)         == True
-- consistent ('box' x y)             == True
-- consistent ('transitiveClosure' x) == True
-- consistent ('scc' x)               == True
-- 'fmap' consistent ('toAcyclic' x)    /= False
-- consistent ('toAcyclicOrd' x)      == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (AAM m) = AM.consistent m && AM.isAcyclic m
