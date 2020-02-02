{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Bipartite.Undirected.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for
-- the motivation behind the library, the underlying theory, and
-- implementation details.
--
-- This module defines the 'AdjacencyMap' data type for undirected bipartite
-- graphs and associated functions. To avoid name clashes with
-- "Algebra.Graph.AdjacencyMap", this module can be imported qualified:
--
-- @
-- import qualified Algebra.Graph.Bipartite.Undirected.AdjacencyMap as Bipartite
-- @
----------------------------------------------------------------------------
module Algebra.Graph.Bipartite.Undirected.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, leftAdjacencyMap, rightAdjacencyMap,

    -- * Basic graph construction primitives
    empty, leftVertex, rightVertex, vertex, edge, overlay, connect, vertices,
    edges, overlays, connects, swap,

    -- * Conversion functions
    toBipartite, toBipartiteWith, fromBipartite, fromBipartiteWith,

    -- * Graph properties
    isEmpty, hasLeftVertex, hasRightVertex, hasVertex, hasEdge, leftVertexCount,
    rightVertexCount, vertexCount, edgeCount, leftVertexList, rightVertexList,
    vertexList, edgeList, leftVertexSet, rightVertexSet, vertexSet, edgeSet,

    -- * Standard families of graphs
    circuit, biclique,

    -- * Algorithms
    OddCycle, detectParts,

    -- * Miscellaneous
    consistent
    ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Either
import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import GHC.Generics

import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Tuple

{-| The 'Bipartite.AdjacencyMap' data type represents an undirected bipartite
graph. The two type parameteters define the types of identifiers of the vertices
of each part.

__Note:__ even if the identifiers and their types for two vertices of different
parts are equal, these vertices are considered to be different. See examples for
more details.

We define a 'Num' instance as a convenient notation for working with bipartite
graphs:

@
0                     == rightVertex 0
'swap' 1                == leftVertex 1
'swap' 1 + 2            == vertices [1] [2]
'swap' 1 * 2            == edge 1 2
'swap' 1 + 2 * 'swap' 3   == overlay (leftVertex 1) (edge 3 2)
'swap' 1 * (2 + 'swap' 3) == connect (leftVertex 1) (vertices [3] [2])
@

__Note:__ the 'Num' instance does not satisfy several "customary laws" of 'Num',
which dictate that 'fromInteger' @0@ and 'fromInteger' @1@ should act as
additive and multiplicative identities, and 'negate' as additive inverse.
Nevertheless, overloading 'fromInteger', '+' and '*' is very convenient when
working with algebraic graphs; we hope that in future Haskell's Prelude will
provide a more fine-grained class hierarchy for algebraic structures, which we
would be able to utilise without violating any laws.

The 'Show' instance is defined using basic graph construction primitives:

@
show empty                 == "empty"
show 1                     == "rightVertex 1"
show ('swap' 2)              == "leftVertex 2"
show (1 + 2)               == "vertices [] [1,2]"
show ('swap' (1 + 2))        == "vertices [1,2] []"
show ('swap' 1 * 2)          == "edge 1 2"
show ('swap' 1 * 2 * 'swap' 3) == "edges [(1,2),(3,2)]"
show ('swap' 1 * 2 + 'swap' 3) == "overlay (leftVertex 3) (edge 1 2)"
@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is commutative, associative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        >       x * y == y * x
        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

    * 'connect' has the same effect as 'overlay' on vertices of one part:

        >  leftVertex x * leftVertex y  ==  leftVertex x + leftVertex y
        > rightVertex x * rightVertex y == rightVertex x + rightVertex y

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        > x + empty == x
        > empty + x == x
        >     x + x == x

    * Absorption and saturation of 'connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively. In
addition, /l/ and /r/ will denote the number of vertices in the left and in the
right part of graph, respectively.
-}
data AdjacencyMap a b = BAM {
    -- | The /adjacency map/ of the left part of the graph: each left vertex is
    -- associated with a set of its right neighbours.
    -- Complexity: /O(1)/ time and memory.
    --
    -- @
    -- leftAdjacencyMap 'empty'           == Map.'Map.empty'
    -- leftAdjacencyMap ('leftVertex' x)  == Map.'Map.singleton' x Set.'Set.empty'
    -- leftAdjacencyMap ('rightVertex' x) == Map.'Map.empty'
    -- leftAdjacencyMap ('edge' x y)      == Map.'Map.singleton' x (Set.'Set.singleton' y)
    -- @
    leftAdjacencyMap :: Map a (Set b),

    -- | The /adjacency map/ of the right part of the graph: each right vertex
    -- is associated with a set of left neighbours.
    -- Complexity: /O(1)/ time and memory.
    --
    -- @
    -- rightAdjacencyMap 'empty'           == Map.'Map.empty'
    -- rightAdjacencyMap ('leftVertex' x)  == Map.'Map.empty'
    -- rightAdjacencyMap ('rightVertex' x) == Map.'Map.singleton' x Set.'Set.empty'
    -- rightAdjacencyMap ('edge' x y)      == Map.'Map.singleton' y (Set.'Set.singleton' x)
    -- @
    rightAdjacencyMap :: Map b (Set a)
    } deriving Generic

-- | __Note:__ this does not satisfy the usual ring laws; see 'AdjacencyMap'
-- for more details.
instance (Ord a, Ord b, Num b) => Num (AdjacencyMap a b) where
    fromInteger = rightVertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance (Ord a, Ord b) => Eq (AdjacencyMap a b) where
    BAM lr1 rl1 == BAM lr2 rl2 = lr1 == lr2 && Map.keysSet rl1 == Map.keysSet rl2

instance (Ord a, Ord b) => Ord (AdjacencyMap a b) where
    compare x y = mconcat
        [ compare (vertexCount x) (vertexCount  y)
        , compare (vertexSet   x) (vertexSet    y)
        , compare (edgeCount   x) (edgeCount    y)
        , compare (edgeSet     x) (edgeSet      y) ]

instance (Ord a, Ord b, Show a, Show b) => Show (AdjacencyMap a b) where
    showsPrec p bam
        | null lvs && null rvs             = showString "empty"
        | null es                          = showParen (p > 10) $ vshow lvs rvs
        | (lvs == lused) && (rvs == rused) = showParen (p > 10) $ eshow es
        | otherwise                        = showParen (p > 10)
                                           $ showString "overlay ("
                                           . veshow (vs \\ used)
                                           . showString ") ("
                                           . eshow es
                                           . showString ")"
      where
        lvs = leftVertexList bam
        rvs = rightVertexList bam
        vs  = vertexList bam
        es  = edgeList bam
        vshow [x] [] = showString "leftVertex " . showsPrec 11 x
        vshow [] [x] = showString "rightVertex " . showsPrec 11 x
        vshow xs ys  = showString "vertices " . showsPrec 11 xs
                     . showString " " . showsPrec 11 ys
        veshow xs      = vshow (lefts xs) (rights xs)
        eshow [(x, y)] = showString "edge " . showsPrec 11 x
                       . showString " " . showsPrec 11 y
        eshow es       = showString "edges " . showsPrec 11 es
        lused = Set.toAscList $ Set.fromAscList [ u | (u, _) <- edgeList bam ]
        rused = Set.toAscList $ Set.fromList    [ v | (_, v) <- edgeList bam ]
        used  = map Left lused ++ map Right rused

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty' empty           == True
-- 'leftAdjacencyMap' empty  == Map.'Map.empty'
-- 'rightAdjacencyMap' empty == Map.'Map.empty'
-- 'hasVertex' x empty       == False
-- @
empty :: AdjacencyMap a b
empty = BAM Map.empty Map.empty

-- | Construct the bipartite graph comprising /a single isolated vertex/ in
-- the left part.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftAdjacencyMap' (leftVertex x)  == Map.'Map.singleton' x Set.'Set.empty'
-- 'rightAdjacencyMap' (leftVertex x) == Map.'Map.empty'
-- 'hasLeftVertex' x (leftVertex y)   == (x == y)
-- 'hasRightVertex' x (leftVertex y)  == False
-- 'hasEdge' x y (leftVertex z)       == False
-- @
leftVertex :: a -> AdjacencyMap a b
leftVertex x = BAM (Map.singleton x Set.empty) Map.empty

-- | Construct the bipartite graph comprising /a single isolated vertex/ in
-- the right part.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftAdjacencyMap' (rightVertex x)  == Map.'Map.empty'
-- 'rightAdjacencyMap' (rightVertex x) == Map.'Map.singleton' x Set.'Set.empty'
-- 'hasLeftVertex' x (rightVertex y)   == False
-- 'hasRightVertex' x (rightVertex y)  == (x == y)
-- 'hasEdge' x y (rightVertex z)       == False
-- @
rightVertex :: b -> AdjacencyMap a b
rightVertex y = BAM Map.empty (Map.singleton y Set.empty)

-- | Construct the bipartite graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- vertex . Left  == 'leftVertex'
-- vertex . Right == 'rightVertex'
-- @
vertex :: Either a b -> AdjacencyMap a b
vertex (Left x)  = leftVertex x
vertex (Right y) = rightVertex y

-- | Construct the bipartite graph comprising /a single edge/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- edge x y                     == 'connect' ('leftVertex' x) ('rightVertex' y)
-- 'leftAdjacencyMap' (edge x y)  == Map.'Map.singleton' x (Set.'Set.singleton' y)
-- 'rightAdjacencyMap' (edge x y) == Map.'Map.singleton' y (Set.'Set.singleton' x)
-- 'hasEdge' x y (edge x y)       == True
-- 'hasEdge' 1 2 (edge 2 1)       == False
-- @
edge :: a -> b -> AdjacencyMap a b
edge x y =
    BAM (Map.singleton x (Set.singleton y)) (Map.singleton y (Set.singleton x))

-- | /Overlay/ two bipartite graphs. This is a commutative, associative and
-- idempotent operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- @
overlay :: (Ord a, Ord b) => AdjacencyMap a b -> AdjacencyMap a b -> AdjacencyMap a b
overlay (BAM lr1 rl1) (BAM lr2 rl2) =
    BAM (Map.unionWith Set.union lr1 lr2) (Map.unionWith Set.union rl1 rl2)

-- | /Connect/ two bipartite graphs, not adding the edges between vertices in
-- the same part. This is a commutative and associative operation with the
-- identity 'empty', which distributes over 'overlay' and obeys the
-- decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices in the arguments: /O(m1 + m2 + l1 * r2 + l2 * r1)/.
--
-- @
-- connect ('leftVertex' x)     ('leftVertex' y)     == 'vertices' [x,y] []
-- connect ('leftVertex' x)     ('rightVertex' y)    == 'edge' x y
-- connect ('rightVertex' x)    ('leftVertex' y)     == 'edge' y x
-- connect ('rightVertex' x)    ('rightVertex' y)    == 'vertices' [] [x,y]
-- connect ('vertices' xs1 ys1) ('vertices' xs2 ys2) == 'overlay' ('biclique' xs1 ys2) ('biclique' xs2 ys1)
-- 'isEmpty'     (connect x y)                     == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y)                     == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y)                     >= 'vertexCount' x
-- 'vertexCount' (connect x y)                     <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y)                     >= 'edgeCount' x
-- 'edgeCount'   (connect x y)                     >= 'leftVertexCount' x * 'rightVertexCount' y
-- 'edgeCount'   (connect x y)                     <= 'leftVertexCount' x * 'rightVertexCount' y + 'rightVertexCount' x * 'leftVertexCount' y + 'edgeCount' x + 'edgeCount' y
-- @
connect :: (Ord a, Ord b) => AdjacencyMap a b -> AdjacencyMap a b -> AdjacencyMap a b
connect (BAM lr1 rl1) (BAM lr2 rl2) = BAM lr rl
  where
    l1 = Map.keysSet lr1
    l2 = Map.keysSet lr2
    r1 = Map.keysSet rl1
    r2 = Map.keysSet rl2
    lr = Map.unionsWith Set.union
        [ lr1, lr2, Map.fromSet (const r2) l1, Map.fromSet (const r1) l2 ]
    rl = Map.unionsWith Set.union
        [ rl1, rl2, Map.fromSet (const l2) r1, Map.fromSet (const l1) r2 ]

-- | Construct the graph comprising two given lists of isolated vertices for
-- each part.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the total
-- length of two lists.
--
-- @
-- vertices [] []                    == 'empty'
-- vertices [x] []                   == 'leftVertex' x
-- vertices [] [x]                   == 'rightVertex' x
-- 'hasLeftVertex'  x (vertices xs ys) == 'elem' x xs
-- 'hasRightVertex' y (vertices xs ys) == 'elem' y ys
-- @
vertices :: (Ord a, Ord b) => [a] -> [b] -> AdjacencyMap a b
vertices ls rs = BAM (Map.fromList [ (l, Set.empty) | l <- ls ])
                     (Map.fromList [ (r, Set.empty) | r <- rs ])

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []            == 'empty'
-- edges [(x,y)]       == 'edge' x y
-- edges               == 'overlays' . 'map' ('uncurry' 'edge')
-- 'hasEdge' x y . edges == 'elem' (x,y)
-- 'edgeCount'   . edges == 'length' . 'nub'
-- @
edges :: (Ord a, Ord b) => [(a, b)] -> AdjacencyMap a b
edges es = BAM (Map.fromListWith Set.union [ (x, Set.singleton y) | (x, y) <- es ])
               (Map.fromListWith Set.union [ (y, Set.singleton x) | (x, y) <- es ])

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
overlays :: (Ord a, Ord b) => [AdjacencyMap a b] -> AdjacencyMap a b
overlays ams = BAM (Map.unionsWith Set.union (map leftAdjacencyMap  ams))
                   (Map.unionsWith Set.union (map rightAdjacencyMap ams))

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == connect x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: (Ord a, Ord b) => [AdjacencyMap a b] -> AdjacencyMap a b
connects = foldr connect empty

-- | Swap parts of a given graph.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- swap 'empty'            == 'empty'
-- swap . 'leftVertex'     == 'rightVertex'
-- swap ('vertices' xs ys) == 'vertices' ys xs
-- swap ('edge' x y)       == 'edge' y x
-- swap . 'edges'          == 'edges' . 'map' Data.Tuple.'Data.Tuple.swap'
-- swap . swap           == 'id'
-- @
swap :: AdjacencyMap a b -> AdjacencyMap b a
swap (BAM lr rl) = BAM rl lr

-- | Construct a bipartite 'AdjacencyMap' from an "Algebra.Graph.AdjacencyMap"
-- with given part identifiers, adding all needed edges to make the graph
-- undirected and removing all edges within the same parts.
-- Complexity: /O(m * log(n))/.
--
-- @
-- toBipartite 'Algebra.Graph.AdjacencyMap.empty'                      == 'empty'
-- toBipartite ('Algebra.Graph.AdjacencyMap.vertex' (Left x))          == 'leftVertex' x
-- toBipartite ('Algebra.Graph.AdjacencyMap.vertex' (Right x))         == 'rightVertex' x
-- toBipartite ('Algebra.Graph.AdjacencyMap.edge' (Left x) (Left y))   == 'vertices' [x,y] []
-- toBipartite ('Algebra.Graph.AdjacencyMap.edge' (Left x) (Right y))  == 'edge' x y
-- toBipartite ('Algebra.Graph.AdjacencyMap.edge' (Right x) (Left y))  == 'edge' y x
-- toBipartite ('Algebra.Graph.AdjacencyMap.edge' (Right x) (Right y)) == 'vertices' [] [x,y]
-- toBipartite ('Algebra.Graph.AdjacencyMap.clique' xs)                == 'uncurry' 'biclique' ('partitionEithers' xs)
-- toBipartite . 'fromBipartite'            == 'id'
-- @
toBipartite :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AdjacencyMap a b
toBipartite m = BAM (Map.fromAscList [ (x, setRights ys) | (Left  x, ys) <- symmetricList ])
                    (Map.fromAscList [ (x, setLefts  ys) | (Right x, ys) <- symmetricList ])
  where
    setRights     = Set.fromAscList . rights . Set.toAscList
    setLefts      = Set.fromAscList . lefts  . Set.toAscList
    symmetricList = Map.toAscList $ AM.adjacencyMap $ AM.symmetricClosure m

-- | Construct a bipartite 'AdjacencyMap' from "Algebra.Graph.AdjacencyMap"
-- with part identifiers obtained from a given function, adding all neeeded
-- edges to make the graph undirected and removing all edges within the same
-- parts.
-- Complexity: /O(m * log(n))/.
--
-- @
-- toBipartiteWith f 'Algebra.Graph.AdjacencyMap.empty' == 'empty'
-- toBipartiteWith Left x  == 'vertices' ('vertexList' x) []
-- toBipartiteWith Right x == 'vertices' [] ('vertexList' x)
-- toBipartiteWith f       == 'toBipartite' . 'Algebra.Graph.AdjacencyMap.gmap' f
-- toBipartiteWith id      == 'toBipartite'
-- @
toBipartiteWith :: (Ord a, Ord b, Ord c) => (a -> Either b c) -> AM.AdjacencyMap a -> AdjacencyMap b c
toBipartiteWith f = toBipartite . AM.gmap f

-- | Construct an 'Algrebra.Graph.AdjacencyMap' from a bipartite 'AdjacencyMap'.
-- Complexity: /O(m * log(n))/.
--
-- @
-- fromBipartite 'empty'          == 'Algebra.Graph.AdjacencyMap.empty'
-- fromBipartite ('leftVertex' x) == 'Algebra.Graph.AdjacencyMap.vertex' (Left x)
-- fromBipartite ('edge' x y)     == 'Algebra.Graph.AdjacencyMap.edges' [(Left x, Right y), (Right y, Left x)]
-- 'toBipartite' . fromBipartite  == 'id'
-- @
fromBipartite :: (Ord a, Ord b) => AdjacencyMap a b -> AM.AdjacencyMap (Either a b)
fromBipartite (BAM lr rl) = AM.fromAdjacencySets $
    [ (Left  x, Set.mapMonotonic Right ys) | (x, ys) <- Map.toAscList lr ] ++
    [ (Right y, Set.mapMonotonic Left  xs) | (y, xs) <- Map.toAscList rl ]

-- | Construct an 'Algrebra.Graph.AdjacencyMap' from a bipartite 'AdjacencyMap'
-- given a way to inject vertices from different parts into the resulting vertex
-- type.
-- Complexity: /O(m * log(n))/.
--
-- @
-- fromBipartiteWith Left Right             == 'fromBipartite'
-- fromBipartiteWith id id ('vertices' xs ys) == 'Algebra.Graph.AdjacencyMap.vertices' (xs ++ ys)
-- fromBipartiteWith id id . 'edges'          == 'Algebra.Graph.AdjacencyMap.symmetricClosure' . 'Algebra.Graph.AdjacencyMap.edges'
-- @
fromBipartiteWith :: Ord c => (a -> c) -> (b -> c) -> AdjacencyMap a b -> AM.AdjacencyMap c
fromBipartiteWith f g (BAM lr rl) = AM.fromAdjacencySets $
    [ (f x, Set.map g ys) | (x, ys) <- Map.toAscList lr ] ++
    [ (g y, Set.map f xs) | (y, xs) <- Map.toAscList rl ]

-- | Check if a graph is empty.
-- Complecity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                 == True
-- isEmpty ('overlay' 'empty' 'empty') == True
-- isEmpty ('vertex' x)            == False
-- isEmpty                       == (==) 'empty'
-- @
isEmpty :: AdjacencyMap a b -> Bool
isEmpty (BAM lr rl) = Map.null lr && Map.null rl

-- | Check if a graph contains a given vertex in the left part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasLeftVertex x 'empty'           == False
-- hasLeftVertex x ('leftVertex' y)  == (x == y)
-- hasLeftVertex x ('rightVertex' y) == False
-- @
hasLeftVertex :: Ord a => a -> AdjacencyMap a b -> Bool
hasLeftVertex x (BAM lr _) = Map.member x lr

-- | Check if a graph contains a given vertex in the right part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasRightVertex x 'empty'           == False
-- hasRightVertex x ('leftVertex' y)  == False
-- hasRightVertex x ('rightVertex' y) == (x == y)
-- @
hasRightVertex :: Ord b => b -> AdjacencyMap a b -> Bool
hasRightVertex y (BAM _ rl) = Map.member y rl

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex . Left  == 'hasLeftVertex'
-- hasVertex . Right == 'hasRightVertex'
-- @
hasVertex :: (Ord a, Ord b) => Either a b -> AdjacencyMap a b -> Bool
hasVertex (Left x)  = hasLeftVertex x
hasVertex (Right y) = hasRightVertex y

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'      == False
-- hasEdge x y ('vertex' z) == False
-- hasEdge x y ('edge' x y) == True
-- hasEdge x y            == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> Bool
hasEdge x y (BAM m _) = (Set.member y <$> Map.lookup x m) == Just True

-- | The number of vertices in the left part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- leftVertexCount 'empty'           == 0
-- leftVertexCount ('leftVertex' x)  == 1
-- leftVertexCount ('rightVertex' x) == 0
-- leftVertexCount ('edge' x y)      == 1
-- leftVertexCount . 'edges'         == 'length' . 'nub' . 'map' 'fst'
-- @
leftVertexCount :: AdjacencyMap a b -> Int
leftVertexCount = Map.size . leftAdjacencyMap

-- | The number of vertices in the right part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- rightVertexCount 'empty'           == 0
-- rightVertexCount ('leftVertex' x)  == 0
-- rightVertexCount ('rightVertex' x) == 1
-- rightVertexCount ('edge' x y)      == 1
-- rightVertexCount . 'edges'         == 'length' . 'nub' . 'map' 'snd'
-- @
rightVertexCount :: AdjacencyMap a b -> Int
rightVertexCount = Map.size . rightAdjacencyMap

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount ('edge' x y) == 2
-- vertexCount x          == 'leftVertexCount' x + 'rightVertexCount' x
-- @
vertexCount :: AdjacencyMap a b -> Int
vertexCount g = leftVertexCount g + rightVertexCount g

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount . 'edges'    == 'length' . 'nub'
-- @
edgeCount :: AdjacencyMap a b -> Int
edgeCount = Map.foldr ((+) . Set.size) 0 . leftAdjacencyMap

-- | The sorted list of vertices of the left part of a given graph.
-- Complexity: /O(l)/ time and memory.
--
-- @
-- leftVertexList 'empty'              == []
-- leftVertexList ('leftVertex' x)     == [x]
-- leftVertexList ('rightVertex' x)    == []
-- leftVertexList . 'flip' 'vertices' [] == 'nub' . 'sort'
-- @
leftVertexList :: AdjacencyMap a b -> [a]
leftVertexList = Map.keys . leftAdjacencyMap

-- | The sorted list of vertices of the right part of a given graph.
-- Complexity: /O(r)/ time and memory.
--
-- @
-- rightVertexList 'empty'           == []
-- rightVertexList ('leftVertex' x)  == []
-- rightVertexList ('rightVertex' x) == [x]
-- rightVertexList . 'vertices' []   == 'nub' . 'sort'
-- @
rightVertexList :: AdjacencyMap a b -> [b]
rightVertexList = Map.keys . rightAdjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory
--
-- @
-- vertexList 'empty'                             == []
-- vertexList ('vertex' x)                        == [x]
-- vertexList ('edge' x y)                        == [Left x, Right y]
-- vertexList ('vertices' ('lefts' xs) ('rights' xs)) == 'nub' ('sort' xs)
-- @
vertexList :: AdjacencyMap a b -> [Either a b]
vertexList g = map Left (leftVertexList g) ++ map Right (rightVertexList g)

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'      == []
-- edgeList ('vertex' x) == []
-- edgeList ('edge' x y) == [(x,y)]
-- edgeList . 'edges'    == 'nub' . 'sort'
-- @
edgeList :: AdjacencyMap a b -> [(a, b)]
edgeList (BAM lr _) = [ (x, y) | (x, ys) <- Map.toAscList lr, y <- Set.toAscList ys ]

-- | The set of vertices of the left part of a given graph.
-- Complexity: /O(l)/ time and memory.
--
-- @
-- leftVertexSet 'empty'              == Set.'Set.empty'
-- leftVertexSet . 'leftVertex'       == Set.'Set.singleton'
-- leftVertexSet . 'rightVertex'      == 'const' Set.'Set.empty'
-- leftVertexSet . 'flip' 'vertices' [] == Set.'Set.fromList'
-- @
leftVertexSet :: AdjacencyMap a b -> Set a
leftVertexSet = Map.keysSet . leftAdjacencyMap

-- | The set of vertices of the right part of a given graph.
-- Complexity: /O(r)/ time and memory.
--
-- @
-- rightVertexSet 'empty'         == Set.'Set.empty'
-- rightVertexSet . 'leftVertex'  == 'const' Set.'Set.empty'
-- rightVertexSet . 'rightVertex' == Set.'Set.singleton'
-- rightVertexSet . 'vertices' [] == Set.'Set.fromList'
-- @
rightVertexSet :: AdjacencyMap a b -> Set b
rightVertexSet = Map.keysSet . rightAdjacencyMap

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'                             == Set.'Set.empty'
-- vertexSet . 'vertex'                          == Set.'Set.singleton'
-- vertexSet ('edge' x y)                        == Set.'Set.fromList' [Left x, Right y]
-- vertexSet ('vertices' ('lefts' xs) ('rights' xs)) == Set.'Set.fromList' xs
-- @
vertexSet :: (Ord a, Ord b) => AdjacencyMap a b -> Set (Either a b)
vertexSet = Set.fromAscList . vertexList

-- | The set of edges of a given graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Data.Set.empty'
-- edgeSet ('vertex' x) == Set.'Data.Set.empty'
-- edgeSet ('edge' x y) == Set.'Data.Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Data.Set.fromList'
-- @
edgeSet :: (Ord a, Ord b) => AdjacencyMap a b -> Set (a, b)
edgeSet = Set.fromAscList . edgeList

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- circuit []                    == 'empty'
-- circuit [(x,y)]               == 'edge' x y
-- circuit [(1,2), (3,4)]        == 'biclique' [1,3] [2,4]
-- circuit [(1,2), (3,4), (5,6)] == 'edges' [(1,2), (3,2), (3,4), (5,4), (5,6), (1,6)]
-- circuit . 'reverse'             == 'swap' . circuit . 'map' Data.Tuple.'Data.Tuple.swap'
-- @
circuit :: (Ord a, Ord b) => [(a, b)] -> AdjacencyMap a b
circuit [] = empty
circuit xs = edges $ xs ++ zip (drop 1 $ cycle as) bs
  where
    (as, bs) = unzip xs

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique [] [] == 'empty'
-- biclique xs [] == 'vertices' xs []
-- biclique [] ys == 'vertices' [] ys
-- biclique xs ys == 'connect' ('vertices' xs []) ('vertices' [] ys)
-- @
biclique :: (Ord a, Ord b) => [a] -> [b] -> AdjacencyMap a b
biclique xs ys = BAM (Map.fromSet (const sys) sxs) (Map.fromSet (const sxs) sys)
  where
    sxs = Set.fromList xs
    sys = Set.fromList ys

data Part = LeftPart | RightPart deriving (Show, Eq)

otherPart :: Part -> Part
otherPart LeftPart  = RightPart
otherPart RightPart = LeftPart

-- | An cycle of odd length. For example, @[1, 2, 3]@ represents the cycle
-- @1 -> 2 -> 3 -> 1@.
type OddCycle a = [a] -- TODO: Make this representation type-safe

-- | Test the bipartiteness of given graph. In case of success, return an
-- 'AdjacencyMap' with the same set of edges and each vertex marked with the
-- part it belongs to. In case of failure, return any cycle of odd length in the
-- graph.
--
-- The returned partition is lexicographically minimal. That is, consider the
-- string of part identifiers for each vertex in ascending order. Then,
-- considering that the identifier of the left part is less then the identifier
-- of the right part, this string is lexicographically minimal of all such
-- strings for all partitions.
--
-- The returned cycle is optimal in the following way: there exists a path that
-- is either empty or ends in a vertex adjacent to the first vertex in the
-- cycle, such that all vertices in @path ++ cycle@ are distinct and
-- @path ++ cycle@ is lexicographically minimal among all such pairs of paths
-- and cycles.
--
-- /Note/: since 'AdjacencyMap' represents __undirected__ bipartite graphs, all
-- edges in the input graph are treated as undirected. See the examples and the
-- correctness property for a clarification.
--
-- It is advised to use 'leftVertexList' and 'rightVertexList' to obtain the
-- partition of the vertices and 'hasLeftVertex' and 'hasRightVertex' to check
-- whether a vertex belongs to a part.
--
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- detectParts 'Algebra.Graph.AdjacencyMap.empty'                                       == Right 'empty'
-- detectParts ('Algebra.Graph.AdjacencyMap.vertex' x)                                  == Right ('leftVertex' x)
-- detectParts ('Algebra.Graph.AdjacencyMap.edge' x x)                                  == Left [x]
-- detectParts ('Algebra.Graph.AdjacencyMap.edge' 1 2)                                  == Right ('edge' 1 2)
-- detectParts (1 * (2 + 3))                               == Right ('edges' [(1,2), (1,3)])
-- detectParts (1 * 2 * 3)                                 == Left [1, 2, 3]
-- detectParts ((1 + 3) * (2 + 4) + 6 * 5)                 == Right ('swap' (1 + 3) * (2 + 4) + 'swap' 5 * 6)
-- detectParts ((1 * 3 * 4) + 2 * (1 + 2))                 == Left [2]
-- detectParts ('Algebra.Graph.AdjacencyMap.clique' [1..10])                            == Left [1, 2, 3]
-- detectParts ('Algebra.Graph.AdjacencyMap.circuit' [1..10])                           == Right ('circuit' [(x, x + 1) | x <- [1,3,5,7,9]])
-- detectParts ('Algebra.Graph.AdjacencyMap.circuit' [1..11])                           == Left [1..11]
-- detectParts ('Algebra.Graph.AdjacencyMap.biclique' [] xs)                            == Right ('vertices' xs [])
-- detectParts ('Algebra.Graph.AdjacencyMap.biclique' ('map' Left (x:xs)) ('map' Right ys)) == Right ('biclique' ('map' Left (x:xs)) ('map' Right ys))
-- 'isRight' (detectParts ('Algebra.Graph.AdjacencyMap.star' x ys))                       == 'notElem' x ys
-- 'isRight' (detectParts ('fromBipartite' ('toBipartite' x)))   == True
-- @
--
-- The correctness of 'detectParts' can be expressed by the following property:
--
-- @
-- let undirected = 'Algebra.Graph.AdjacencyMap.symmetricClosure' input in
-- case detectParts input of
--     Left cycle -> 'mod' (length cycle) 2 == 1 && 'Algebra.Graph.AdjacencyMap.isSubgraphOf' ('Algebra.Graph.AdjacencyMap.circuit' cycle) undirected
--     Right result -> 'Algebra.Graph.AdjacencyMap.gmap' 'Data.Either.Extra.fromEither' ('fromBipartite' result) == undirected
-- @
detectParts :: Ord a => AM.AdjacencyMap a -> Either (OddCycle a) (AdjacencyMap a a)
detectParts x = case runState (runMaybeT dfs) Map.empty of
    (Nothing, m) -> Right $ toBipartiteWith (toEither m) g
    (Just c,  _) -> Left  $ oddCycle c
  where
    -- g :: AM.AdjacencyMap a
    g = AM.symmetricClosure x

    -- type PartMap a = Map a Part
    -- type PartMonad a = MaybeT (State (PartMap a)) [a]
    -- dfs :: PartMonad a
    dfs = asum [ processVertex v | v <- AM.vertexList g ]

    -- processVertex :: a -> PartMonad a
    processVertex v = do m <- get
                         guard (Map.notMember v m)
                         inVertex LeftPart v

    -- inVertex :: Part -> a -> PartMonad a
    inVertex p v = ((:) v) <$> do modify (Map.insert v p)
                                  let q = otherPart p
                                  asum [ onEdge q u | u <- Set.toAscList (AM.postSet v g) ]

    {-# INLINE onEdge #-}
    -- onEdge :: Part -> a -> PartMonad a
    onEdge p v = do m <- get
                    case Map.lookup v m of
                        Nothing -> inVertex p v
                        Just q  -> do guard (p /= q)
                                      return [v]

    -- toEither :: PartMap a -> a -> Either a a
    toEither m v = case fromJust (Map.lookup v m) of
                       LeftPart  -> Left  v
                       RightPart -> Right v

    -- oddCycle :: [a] -> [a]
    oddCycle c = init $ dropWhile (/= last c) c

-- | Check that the internal graph representation is consistent, i.e. that all
-- edges that are present in the 'leftAdjacencyMap' are also present in the
-- 'rightAdjacencyMap' map. It should be impossible to create an inconsistent
-- adjacency map, and we use this function in testing.
--
-- @
-- consistent 'empty'           == True
-- consistent ('vertex' x)      == True
-- consistent ('edge' x y)      == True
-- consistent ('edges' x)       == True
-- consistent ('toBipartite' x) == True
-- consistent ('swap' x)        == True
-- consistent ('circuit' x)     == True
-- consistent ('biclique' x y)  == True
-- @
consistent :: (Ord a, Ord b) => AdjacencyMap a b -> Bool
consistent (BAM lr rl) = edgeList lr == sort (map Data.Tuple.swap $ edgeList rl)
  where
    edgeList lr = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]
