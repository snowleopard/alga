{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Bipartite.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for
-- the motivation behind the library, the underlying theory, and
-- implementation details.
--
-- This module defines the 'AdjacencyMap' data type for bipartite graphs and
-- basic associated functions.
----------------------------------------------------------------------------
module Algebra.Graph.Bipartite.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, leftAdjacencyMap, rightAdjacencyMap,

    -- * Basic graph construction primitives
    empty, leftVertex, rightVertex, vertex, edge, overlay, connect,
    vertices, edges, overlays, connects, swap,

    -- * Conversion functions
    toBipartite, toBipartiteWith, fromBipartite, fromGraph,

    -- * Graph properties
    isEmpty, hasEdge, hasLeftVertex, hasRightVertex, hasVertex, leftVertexCount,
    rightVertexCount, vertexCount, edgeCount, leftVertexList, rightVertexList,
    vertexList, edgeList, leftVertexSet, rightVertexSet, vertexSet, edgeSet,

    -- * Standard families of graphs
    circuit, biclique,

    -- * Testing bipartiteness
    OddCycle, detectParts,

    -- * Miscellaneous
    consistent,
    ) where

import Control.Monad             (guard)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State       (State, runState, modify, get)
import Data.Either               (lefts, rights)
import Data.Foldable             (asum)
import Data.List                 (sort, (\\))
import Data.Maybe                (fromJust)
import GHC.Generics

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Tuple

{-| The 'Bipartite.AdjacencyMap' data type represents an __undirected__
bipartite graph. The two type parameteters define the types of identifiers of
the vertices of each part.

__Note:__ even if the identifiers and their types for two vertices of
different parts are equal, these vertices are considered to be different.
See examples for more details.

We define a 'Num' instance as a convenient notation for working with bipartite
graphs:

@
0                         == rightVertex 0
'swap' 1                    == leftVertex 1
'swap' 1 + 2                == vertices [1] [2]
('swap' 1) * 2              == edge 1 2
('swap' 1) + 2 * ('swap' 3)   == overlay (leftVertex 1) (edge 3 2)
('swap' 1) * (2 + ('swap' 3)) == connect (leftVertex 1) (vertices [3] [2])
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
show (empty)                   == "empty"
show 1                         == "rightVertex 1"
show ('swap' 2)                  == "leftVertex 2"
show (1 + 2)                   == "vertices [] [1,2]"
show ('swap' (1 + 2))            == "vertices [1,2] []"
show ('swap' 1 * 2)              == "edge 1 2"
show ('swap' 1 * 2 * 'swap' 3)     == "edges [(1,2),(3,2)]"
show ('swap' 1 * 2 + 'swap' 3)     == "overlay (leftVertex 3) (edge 1 2)"
show ('swap' 1 * 2 + 'swap' 3 + 4) == "overlay (vertices [3] [4]) (edge 1 2)"
@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is commutative, associative and has
    'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        >       x * y == y * x
        > x * (y * z) == (x * y) * z

    * 'connect' distributes over
    'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

    * 'connect' has the same effect as 'overlay' on vertices of one part:

        > (leftVertex x)  * (leftVertex y)  == (leftVertex x)  + (leftVertex y)
        > (rightVertex x) * (rightVertex y) == (rightVertex x) + (rightVertex y)

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty'
    as the identity and is idempotent:

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
    -- | The /adjacency map/ of the left part of the graph: each vertex is
    -- associated with a set of its neighbours. Complexity: /O(1)/ time and
    -- memory.
    --
    -- @
    -- leftAdjacencyMap 'empty'                    == Map.'Map.empty'
    -- leftAdjacencyMap ('leftVertex' 1)           == Map.'Map.singleton' 1 Set.'Set.empty'
    -- leftAdjacencyMap ('rightVertex' 1)          == Map.'Map.empty'
    -- leftAdjacencyMap ('edge' 1 1)               == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- leftAdjacencyMap ('edge' 1 "a")             == Map.'Map.singleton' 1 (Set.'Set.singleton' "a")
    -- leftAdjacencyMap ('edges' [(1, 1), (1, 2)]) == Map.'Map.singleton' 1 (Set.'Set.fromAscList' [1, 2])
    -- @
    leftAdjacencyMap :: Map.Map a (Set.Set b),

    -- | The inverse map for 'leftAdjacencyMap'. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- rightAdjacencyMap 'empty'                    == Map.'Map.empty'
    -- rightAdjacencyMap ('leftVertex' 1)           == Map.'Map.empty'
    -- rightAdjacencyMap ('rightVertex' 1)          == Map.'Map.singleton' 1 Set.'Set.empty'
    -- rightAdjacencyMap ('edge' 1 1)               == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- rightAdjacencyMap ('edge' 1 "a")             == Map.'Map.singleton' "a" (Set.'Set.singleton' 1)
    -- rightAdjacencyMap ('edges' [(1, 1), (1, 2)]) == Map.'Map.fromAscList' [(1, Set.'Set.singleton' 1), (2, Set.'Set.singleton' 1)]
    -- @
    rightAdjacencyMap :: Map.Map b (Set.Set a)
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
    (BAM lr1 rl1) == (BAM lr2 rl2) = (lr1 == lr2) && (Map.keysSet rl1 == Map.keysSet rl2)

instance (Ord a, Ord b, Show a, Show b) => Show (AdjacencyMap a b) where
    showsPrec p bam
        | null lvs && null rvs             = showString "empty"
        | null es                          = showParen (p > 10) $ vshow lvs rvs
        | (lvs == lused) && (rvs == rused) = showParen (p > 10) $ eshow es
        | otherwise                    = showParen (p > 10) $
                                               showString "overlay (" .
                                               veshow (vs \\ used) .
                                               showString ") (" .
                                               eshow es .
                                               showString ")"
      where
        lvs = leftVertexList bam
        rvs = rightVertexList bam
        vs = vertexList bam
        es = edgeList bam
        vshow [x] [] = showString "leftVertex " . showsPrec 11 x
        vshow [] [x] = showString "rightVertex " . showsPrec 11 x
        vshow xs ys = showString "vertices " . showsPrec 11 xs .
                      showString " " . showsPrec 11 ys
        veshow xs = vshow (lefts xs) (rights xs)
        eshow [(x, y)] = showString "edge " . showsPrec 11 x .
                         showString " " . showsPrec 11 y
        eshow es       = showString "edges " . showsPrec 11 es
        lused = Set.toAscList $ Set.fromAscList [ u | (u, _) <- edgeList bam ]
        rused = Set.toAscList $ Set.fromList    [ v | (_, v) <- edgeList bam ]
        used = (map Left lused) ++ (map Right rused)

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
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
-- 'hasEdge' x y (leftVertex z)       == False
-- 'hasLeftVertex' x (leftVertex x)   == True
-- 'hasRightVertex' x (leftVertex x)  == False
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
-- 'hasEdge' x y (rightVertex y)       == False
-- 'hasLeftVertex' x (rightVertex x)   == False
-- 'hasRightVertex' x (rightVertex x)  == True
-- @
rightVertex :: b -> AdjacencyMap a b
rightVertex y = BAM Map.empty (Map.singleton y Set.empty)

-- | Construct the bipartite graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- vertex (Left x)                == 'leftVertex' x
-- vertex (Right x)               == 'rightVertex' x
-- 'hasEdge' x y (vertex (Left x))  == False
-- 'hasEdge' x y (vertex (Right y)) == False
-- @
vertex :: Either a b -> AdjacencyMap a b
vertex (Left x)  = leftVertex x
vertex (Right y) = rightVertex y

-- | Construct the bipartite graph comprising /a single edge/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftAdjacencyMap' (edge x y)  == Map.'Map.singleton' x (Set.'Set.singleton' y)
-- 'rightAdjacencyMap' (edge x y) == Map.'Map.singleton' y (Set.'Set.singleton' x)
-- 'hasEdge' x y (edge x y)       == True
-- 'hasEdge' 1 1 (edge 1 1)       == True
-- 'hasEdge' 2 1 (edge 1 2)       == False
-- @
edge :: a -> b -> AdjacencyMap a b
edge x y = BAM (Map.singleton x (Set.singleton y)) (Map.singleton y (Set.singleton x))

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
overlay (BAM lr1 rl1) (BAM lr2 rl2) = BAM (Map.unionWith Set.union lr1 lr2) (Map.unionWith Set.union rl1 rl2)

-- | /Connect/ two bipartite graphs, not adding the edges between vertices in
-- the same part. This is a commutative and associative operation with the
-- identity 'empty', which distributes over 'overlay' and obeys the
-- decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices in the arguments: /m = O(m1 + m2 + l1 * r2 + l2 * r1)/.
--
-- @
-- connect ('leftVertex' 1) ('rightVertex' "a")      == 'edge' 1 "a"
-- connect ('leftVertex' 1) ('rightVertex' 1)        == 'edge' 1 1
-- connect ('leftVertex' 1) ('leftVertex' 2)         == 'vertices' [1, 2] []
-- connect ('vertices' [1] [4]) ('vertices' [2] [3]) == 'edges' [(1, 3), (2, 4)]
-- 'isEmpty'     (connect x y)                     == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y)                     == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y)                     >= 'vertexCount' x
-- 'vertexCount' (connect x y)                     <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y)                     >= 'edgeCount' x
-- 'edgeCount'   (connect x y)                     >= 'edgeCount' y
-- 'edgeCount'   (connect x y)                     >= 'leftVertexCount' x * 'rightVertexCount' y
-- 'edgeCount'   (connect x y)                     <= 'leftVertexCount' x * 'rightVertexCount' y + 'rightVertexCount' x * 'leftVertexCount' y + 'edgeCount' x + 'edgeCount' y
-- @
connect :: (Ord a, Ord b) => AdjacencyMap a b -> AdjacencyMap a b -> AdjacencyMap a b
connect (BAM lr1 rl1) (BAM lr2 rl2) = BAM lr rl
    where
        lr = Map.unionsWith Set.union $
            [ lr1, lr2
            , Map.fromSet (const $ Map.keysSet rl2) (Map.keysSet lr1)
            , Map.fromSet (const $ Map.keysSet rl1) (Map.keysSet lr2)
            ]
        rl = Map.unionsWith Set.union $
            [ rl1, rl2
            , Map.fromSet (const $ Map.keysSet lr2) (Map.keysSet rl1)
            , Map.fromSet (const $ Map.keysSet lr1) (Map.keysSet rl2)
            ]

-- | Construct the graph comprising two given lists of isolated vertices for
-- each part.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the total
-- length of two lists.
--
-- @
-- vertices [] []                      == 'empty'
-- vertices [x] []                     == 'leftVertex' x
-- vertices [] [x]                     == 'rightVertex' x
-- 'hasLeftVertex'  x (vertices ys zs) == 'elem' x ys
-- 'hasRightVertex' x (vertices ys zs) == 'elem' x zs
-- @
vertices :: (Ord a, Ord b) => [a] -> [b] -> AdjacencyMap a b
vertices ls rs = BAM (Map.fromList $ map ((flip (,)) Set.empty) ls) (Map.fromList $ map ((flip (,)) Set.empty) rs)

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges :: (Ord a, Ord b) => [(a, b)] -> AdjacencyMap a b
edges es = BAM (Map.fromListWith Set.union (map (onRight Set.singleton) es)) $
                Map.fromListWith Set.union (map (onRight Set.singleton) (map Data.Tuple.swap es))
    where
        onRight f (x, y) = (x, f y)

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
overlays ams = BAM (Map.unionsWith Set.union (map leftAdjacencyMap ams)) $
                    Map.unionsWith Set.union (map rightAdjacencyMap ams)

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
-- swap 'empty'        == 'empty'
-- swap . 'leftVertex' == rightVertex
-- swap . 'vertices'   == flip 'vertices'
-- swap ('edge' 1 "a") == 'edge' "a" 1
-- swap . 'edges'      == 'edges' . map Data.Tuple.'Data.Tuple.swap'
-- swap . swap       == id
-- @
swap :: AdjacencyMap a b -> AdjacencyMap b a
swap (BAM lr rl) = BAM rl lr

-- | Construct a bipartite 'AdjacencyMap' from "Algebra.Graph.AdjacencyMap"
-- with given part identifiers, adding all needed edges to make the graph
-- undirected and removing all edges inside one part.
-- Complexity: /O(m log(n))/.
--
-- @
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.empty')                     == 'empty'
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right 1))   == 'edge' 1 1
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.edge' (Left 1) (Left 1))    == 'empty'
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right "a")) == 'edge' 1 "a"
-- @
toBipartite :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AdjacencyMap a b
toBipartite m = BAM (Map.fromAscList [ (u, setRights vs) | (Left  u, vs) <- symmetricList])
                    (Map.fromAscList [ (u, setLefts  vs) | (Right u, vs) <- symmetricList])
    where
        setRights     = Set.fromAscList . rights . Set.toAscList
        setLefts      = Set.fromAscList . lefts  . Set.toAscList
        symmetricList = Map.toAscList $ AM.adjacencyMap $ AM.symmetricClosure m

-- | Construct a bipartite 'AdjacencyMap' from "Algebra.Graph.AdjacencyMap"
-- with part identifiers obtained from a given function, adding all neeeded
-- edges to make the graph undirected and removing all edges inside one part.
-- Complexity: /O(m log(n))/
--
-- @
-- toBipartiteWith f Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.empty' == 'empty'
-- toBipartiteWith Left  x  == 'empty'
-- toBipartiteWith Right x  == 'empty'
-- toBipartiteWith f        == 'toBiparitite' . Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.gmap' f
-- toBipartiteWith id       == 'toBipartite'
-- @
toBipartiteWith :: (Ord a, Ord b, Ord c) => (a -> Either b c) -> AM.AdjacencyMap a -> AdjacencyMap b c
toBipartiteWith f = toBipartite . AM.gmap f

-- | Construct an 'Algrebra.Graph.AdjacencyMap' from a bipartite
-- 'AdjacencyMap'.
-- Complexity: /O(m log(n))/.
--
-- @
-- fromBipartite 'empty'          == 'Algebra.Graph.AdjacencyMap.empty'
-- fromBipartite ('leftVertex' 1) == 'Algebra.Graph.AdjacencyMap.vertex' (Left 1)
-- fromBipartite ('edge' 1 2)     == 'Algebra.Graph.AdjacencyMap.edges' [(Left 1, Right 2), (Right 2, Left 1)]
-- @
fromBipartite :: (Ord a, Ord b) => AdjacencyMap a b -> AM.AdjacencyMap (Either a b)
fromBipartite (BAM lr rl) = AM.fromAdjacencySets $
    [ (Left  u, Set.map Right vs) | (u, vs) <- Map.toAscList lr ] ++
    [ (Right v, Set.map Left  us) | (v, us) <- Map.toAscList rl ]

-- | Construct a bipartite 'AdjacencyMap' from a 'Algebra.Graph.Graph' with
-- given part identifiers, adding all needed edges to make the graph undirected
-- and removing all edges inside one part.
-- Complexity: /O(m log n)/.
--
-- @
-- fromGraph (Algebra.Graph.'Algebra.Graph.empty')                     == 'empty'
-- fromGraph (Algebra.Graph.'Algebra.Graph.edge' (Left 1) (Right 1))   == 'edge' 1 1
-- fromGraph (Algebra.Graph.'Algebra.Graph.edge' (Left 1) (Right "a")) == 'edge' 1 "a"
-- fromGraph (Algebra.Graph.'Algebra.Graph.edge' (Left 1) (Left 2))    == 'empty'
-- @
fromGraph :: (Ord a, Ord b) => G.Graph (Either a b) -> AdjacencyMap a b
fromGraph = toBipartite . (G.foldg AM.empty AM.vertex AM.overlay AM.connect)

internalEdgeList :: Map.Map a (Set.Set b) -> [(a, b)]
internalEdgeList lr = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

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

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'                                == False
-- hasEdge x y ('edge' x y)                           == True
-- hasEdge 2 3 ('edge' 1 2)                           == False
-- hasEdge x y ('overlay' z ('edge' x y))               == True
-- hasEdge 1 2 ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Left 2))) == False
-- @
hasEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> Bool
hasEdge u v (BAM m _) = (Set.member v <$> (u `Map.lookup` m)) == Just True

-- | Check if a graph contains a given vertex in the left part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasLeftVertex x 'empty'           == False
-- hasLeftVertex x ('leftVertex' x)  == True
-- hasLeftVertex x ('rightVertex' x) == False
-- hasLeftVertex 1 ('leftVertex' 2)  == False
-- @
hasLeftVertex :: Ord a => a -> AdjacencyMap a b -> Bool
hasLeftVertex x (BAM lr _) = x `Map.member` lr

-- | Check if a graph contains a given vertex in the right part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasRightVertex x 'empty'           == False
-- hasRightVertex x ('rightVertex' x) == True
-- hasRightVertex x ('leftVertex' x)  == False
-- hasRightVertex 1 ('rightVertex' 2) == False
-- @
hasRightVertex :: Ord b => b -> AdjacencyMap a b -> Bool
hasRightVertex y (BAM _ rl) = y `Map.member` rl

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'                   == False
-- hasVertex (Right x) ('rightVertex' x) == True
-- hasVertex (Right x) ('leftVertex' x)  == False
-- hasVertex (Left 1) ('leftVertex' 2)   == False
-- hasVertex . Left                    == 'hasLeftVertex'
-- hasVertex . Right                   == 'hasRightVertex'
-- @
hasVertex :: (Ord a, Ord b) => Either a b -> AdjacencyMap a b -> Bool
hasVertex (Left x)  = hasLeftVertex x
hasVertex (Right y) = hasRightVertex y

-- | The number of vertices in the left part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- leftVertexCount 'empty'           == 0
-- leftVertexCount ('leftVertex' x)  == 1
-- leftVertexCount ('rightVertex' x) == 0
-- leftVertexCount . 'edges'         == 'length' . 'Data.List.nub' . 'map' 'fst'
-- @
leftVertexCount :: AdjacencyMap a b -> Int
leftVertexCount = Map.size . leftAdjacencyMap

-- | The number of vertices in the right part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- rightVertexCount 'empty'           == 0
-- rightVertexCount ('rightVertex' x) == 1
-- rightVertexCount ('leftVertex' x)  == 0
-- rightVertexCount . 'edges'         == 'length' . 'Data.List.nub' . 'map' 'snd'
-- @
rightVertexCount :: AdjacencyMap a b -> Int
rightVertexCount = Map.size . rightAdjacencyMap

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'           == 0
-- vertexCount ('leftVertex' x)  == 1
-- vertexCount ('rightVertex' x) == 1
-- vertexCount x               == 'leftVertexCount' x + 'rightVertexCount' x
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
-- edgeCount . 'edges'    == 'length' . 'Data.List.nub'
-- @
edgeCount :: AdjacencyMap a b -> Int
edgeCount = Map.foldr ((+) . Set.size) 0 . leftAdjacencyMap

-- | The sorted list of vertices of the left part of a given graph.
-- Complexity: /O(l)/ time and memory.
--
-- @
-- leftVertexList 'empty'                == []
-- leftVertexList ('leftVertex' x)       == [x]
-- leftVertexList ('rightVertex' x)      == []
-- leftVertexList . ('flip' 'vertices') [] == 'Data.List.nub' . 'Data.List.sort'
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
-- rightVertexList . 'vertices' []   == 'Data.List.nub' . 'Data.List.sort'
-- @
rightVertexList :: AdjacencyMap a b -> [b]
rightVertexList = Map.keys . rightAdjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory
--
-- @
-- vertexList 'empty'                             == []
-- vertexList ('vertex' x)                        == [x]
-- vertexList (vertices ('Data.Either.lefts' vs) ('Data.Either.rights' vs)) == 'Data.List.nub' ('Data.List.sort' vs)
-- @
vertexList :: AdjacencyMap a b -> [Either a b]
vertexList g = (map Left $ leftVertexList g) ++ (map Right $ rightVertexList g)

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'      == []
-- edgeList ('vertex' x) == []
-- edgeList ('edge' x y) == [(x, y)]
-- edgeList . 'edges'    == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: AdjacencyMap a b -> [(a, b)]
edgeList (BAM lr _) = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

-- | The set of vertices of the left part of a given graph.
-- Complexity: /O(l)/ time and memory.
--
-- @
-- leftVertexSet 'empty'                == Set.'Data.Set.empty'
-- leftVertexSet . 'leftVertex'         == Set.'Data.Set.singleton'
-- leftVertexSet . 'rightVertex'        == 'const' Set.'Data.Set.empty'
-- leftVertexSet . ('flip' 'vertices') [] == Set.'Data.Set.fromList'
-- @
leftVertexSet :: AdjacencyMap a b -> Set.Set a
leftVertexSet = Map.keysSet . leftAdjacencyMap

-- | The set of vertices of the right part of a given graph.
-- Complexity: /O(r)/ time and memory.
--
-- @
-- rightVertexSet 'empty'         == Set.'Data.Set.empty'
-- rightVertexSet . 'leftVertex'  == 'const' Set.'Data.Set.empty'
-- rightVertexSet . 'rightVertex' == Set.'Data.Set.singleton'
-- rightVertexSet . 'vertices' [] == Set.'Data.Set.fromList'
-- @
rightVertexSet :: AdjacencyMap a b -> Set.Set b
rightVertexSet = Map.keysSet . rightAdjacencyMap

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'                             == Set.'Data.Set.empty'
-- vertexSet . 'vertex'                          == Set.'Data.Set.singleton'
-- vertexSet ('vertices' ('Data.Either.lefts' vs) ('Data.Either.rights' vs)) == Set.'Data.Set.fromList' vs
-- @
vertexSet :: (Ord a, Ord b) => AdjacencyMap a b -> Set.Set (Either a b)
vertexSet = Set.fromAscList . vertexList

-- | The set of edges of a given graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Data.Set.empty'
-- edgeSet ('vertex' x) == Set.'Data.Set.empty'
-- edgeSet ('edge' x y) == Set.'Data.Set.singleton' (x, y)
-- edgeSet . 'edges'    == Set.'Data.Set.fromList'
-- @
edgeSet :: (Ord a, Ord b) => AdjacencyMap a b -> Set.Set (a, b)
edgeSet = Set.fromAscList . edgeList

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- circuit []                       == 'empty'
-- circuit [(x, y)]                 == 'edge' x y
-- circuit [(x, y), (z, w)]         == 'biclique' [x, z] [y, w]
-- circuit [(1, 2), (3, 4), (5, 6)] == swap 1 * (2 + 6) + swap 3 * (2 + 4) + swap 5 * (6 + 2)
-- circuit . 'reverse'              == 'swap' . circuit . 'map' 'Data.Tuple.swap'
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
biclique xs ys = let sxs = Set.fromList xs
                     sys = Set.fromList ys
                  in BAM (Map.fromSet (const sys) sxs)
                         (Map.fromSet (const sxs) sys)

data Part = LeftPart | RightPart
    deriving (Show, Eq)

otherPart :: Part -> Part
otherPart LeftPart  = RightPart
otherPart RightPart = LeftPart

type PartMap a = Map.Map a Part
type PartMonad a = MaybeT (State (PartMap a)) [a]

-- | An odd cycle. For example, @[1, 2, 3]@ represents the cycle 1 → 2 → 3 → 1.
type OddCycle a = [a] -- TODO: Make this representation type-safe

neighbours :: Ord a => a -> AM.AdjacencyMap a -> [a]
neighbours v = Set.toAscList . AM.postSet v

-- | Test bipartiteness of given graph. In case of success, return an
-- 'AdjacencyMap' with the same set of edges and each vertex marked with the
-- part it belongs to. In case of failure, return any odd cycle in the graph.
--
-- The returned partition is lexicographicaly minimal. That is, consider the
-- string of part identifiers for each vertex in ascending order. Then,
-- considering that the identifier of the left part is less then the identifier
-- of the right part, this string is lexicographically minimal of all such
-- strings for all partitions.
--
-- The returned odd cycle is optimal in the following way: there exists a path
-- that is either empty or ends in a vertex adjacent to the first vertex in the
-- cycle, such that all vertices in @path ++ cycle@ are distinct and
-- @path ++ cycle@ is lexicographically minimal among all such pairs of odd
-- cycles and paths.
--
-- /Note/: as 'AdjacencyMap' only represents __undirected__ bipartite graphs,
-- all edges in the input graph are assumed to be bidirected and all edges in
-- the output 'AdjacencyMap' are bidirected.
--
-- It is advised to use 'leftVertexList' and 'rightVertexList' to obtain the
-- partition of the vertices and 'hasLeftVertex' and 'hasRightVertex' to check
-- whether a vertex belongs to a part.
--
-- Complexity: /O((n + m) log(n))/ time and /O(n + m)/ memory.
--
-- @
-- detectParts 'Algebra.Graph.AdjacencyMap.empty'                                       == Right 'empty'
-- detectParts ('Algebra.Graph.AdjacencyMap.vertex' x)                                  == Right ('leftVertex' x)
-- detectParts (1 * (2 + 3))                               == Right ('edges' [(1, 2), (1, 3)])
-- detectParts ((1 + 3) * (2 + 4) + 6 * 5)                 == Right ('swap' (1 + 3) * (2 + 4) + 'swap' 5 * 6)
-- detectParts ('Algebra.Graph.AdjacencyMap.edge' 1 1)                                  == Left [1]
-- detectParts ('Algebra.Graph.AdjacencyMap.edge' 1 2)                                  == Right ('edge' 1 2)
-- detectParts (1 * 2 * 3)                                 == Left [1, 2, 3]
-- detectParts ((1 * 3 * 4) + 2 * (1 + 2))                 == Left [2]
-- detectParts ('Algebra.Graph.AdjacencyMap.clique' [1..10])                            == Left [1, 2, 3]
-- detectParts ('Algebra.Graph.AdjacencyMap.circuit' [1..11])                           == Left [1..11]
-- detectParts ('Algebra.Graph.AdjacencyMap.circuit' [1..10])                           == Right ('circuit' [(2 * x - 1, 2 * x) | x <- [1..5]])
-- detectParts ('Algebra.Graph.AdjacencyMap.biclique' [] xs)                            == Right (vertices xs [])
-- detectParts ('Algebra.Graph.AdjacencyMap.biclique' (map Left (x:xs)) (map Right ys)) == Right ('biclique' (map Left (x:xs)) (map Right ys))
-- 'Data.Either.isRight' (detectParts ('Algebra.Graph.AdjacencyMap.star' x ys))                       == not (elem x ys)
-- 'Data.Either.isRight' (detectParts ('fromBipartite' ('toBipartite' x)))   == True
-- @
detectParts :: forall a. Ord a => AM.AdjacencyMap a -> Either (OddCycle a) (AdjacencyMap a a)
detectParts x = case runState (runMaybeT $ dfs) Map.empty of
                     (Nothing, m) -> Right $ toBipartiteWith (toEither m) g
                     (Just c,  _) -> Left  $ oddCycle c
    where
        g :: AM.AdjacencyMap a
        g = AM.symmetricClosure x

        dfs :: PartMonad a
        dfs = asum [ processVertex v | v <- AM.vertexList g ]

        {-# INLINE onEdge #-}
        onEdge :: Part -> a -> PartMonad a
        onEdge p v = do m <- get
                        case v `Map.lookup` m of
                             Nothing -> inVertex p v
                             Just q  -> do guard (p /= q)
                                           return [v]

        inVertex :: Part -> a -> PartMonad a
        inVertex p v = ((:) v) <$> do modify (Map.insert v p)
                                      let q = otherPart p
                                      asum [ onEdge q u | u <- neighbours v g ]

        processVertex :: a -> PartMonad a
        processVertex v = do m <- get
                             guard (v `Map.notMember` m)
                             inVertex LeftPart v

        toEither :: PartMap a -> a -> Either a a
        toEither m v = case fromJust (v `Map.lookup` m) of
                            LeftPart  -> Left  v
                            RightPart -> Right v

        oddCycle :: [a] -> [a]
        oddCycle c = init $ dropUntil (last c) c

        dropUntil :: a -> [a] -> [a]
        dropUntil _ []        = []
        dropUntil x ys@(y:yt) | y == x    = ys
                              | otherwise = dropUntil x yt


-- | Check that the internal graph representation is consistent, i.e. that all
-- edges that are present in the 'leftAdjacencyMap' are present in the
-- 'rightAdjacencyMap' map.
--
-- @
-- consistent 'empty'           == True
-- consistent ('vertex' x)      == True
-- consistent ('edge' x y)      == True
-- consistent ('edges' x)       == True
-- consistent ('fromGraph' x)   == True
-- consistent ('toBipartite' x) == True
-- consistent ('swap' x)        == True
-- consistent ('circuit' x)     == True
-- consistent ('biclique' x y)  == True
-- @
consistent :: (Ord a, Ord b) => AdjacencyMap a b -> Bool
consistent (BAM lr rl) = internalEdgeList lr == sort (map Data.Tuple.swap $ internalEdgeList rl)
