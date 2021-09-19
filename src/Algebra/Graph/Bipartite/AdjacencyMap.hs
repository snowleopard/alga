----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Bipartite.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2021
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
-- graphs and associated functions. See
-- "Algebra.Graph.Bipartite.AdjacencyMap.Algorithm" for basic bipartite graph
-- algorithms.
--
-- To avoid name clashes with "Algebra.Graph.AdjacencyMap", this module can be
-- imported qualified:
--
-- @
-- import qualified Algebra.Graph.Bipartite.AdjacencyMap as Bipartite
-- @
----------------------------------------------------------------------------
module Algebra.Graph.Bipartite.AdjacencyMap (
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
    leftAdjacencyList, rightAdjacencyList,

    -- * Standard families of graphs
    List (..), evenList, oddList, path, circuit, biclique, star, stars, mesh,

    -- * Graph transformation
    removeLeftVertex, removeRightVertex, removeEdge, bimap,

    -- * Graph composition
    box, boxWith,

    -- * Miscellaneous
    consistent
    ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Either
import Data.Foldable (asum)
import Data.List ((\\), sort)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import GHC.Exts (IsList(..))
import GHC.Generics

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Tuple

{-| The 'Bipartite.AdjacencyMap' data type represents an undirected bipartite
graph. The two type parameters determine the types of vertices of each part. If
the types coincide, the vertices of the left part are still treated as disjoint
from the vertices of the right part. See examples for more details.

We define a 'Num' instance as a convenient notation for working with bipartite
graphs:

@
0                     == 'rightVertex' 0
'swap' 1                == 'leftVertex' 1
'swap' 1 + 2            == 'vertices' [1] [2]
'swap' 1 * 2            == 'edge' 1 2
'swap' 1 + 2 * 'swap' 3   == 'overlay' ('leftVertex' 1) ('edge' 3 2)
'swap' 1 * (2 + 'swap' 3) == 'connect' ('leftVertex' 1) ('vertices' [3] [2])
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

The 'Eq' instance satisfies all axioms of undirected bipartite algebraic graphs:

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

    * 'connect' has the same effect as 'overlay' on vertices of the same part:

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
will denote the number of vertices and edges of the graph, respectively. In
addition, /l/ and /r/ will denote the number of vertices in the left and right
parts of the graph, respectively.
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
    -- is associated with a set of its left neighbours.
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
    BAM ab1 ba1 == BAM ab2 ba2 = ab1 == ab2 && Map.keysSet ba1 == Map.keysSet ba2

instance (Ord a, Ord b) => Ord (AdjacencyMap a b) where
    compare x y = mconcat
        [ compare (vertexCount x) (vertexCount y)
        , compare (vertexSet   x) (vertexSet   y)
        , compare (edgeCount   x) (edgeCount   y)
        , compare (edgeSet     x) (edgeSet     y) ]

instance (Ord a, Ord b, Show a, Show b) => Show (AdjacencyMap a b) where
    showsPrec p g
        | null as && null bs             = showString "empty"
        | null es                        = showParen (p > 10) $ vShow as bs
        | (as == aUsed) && (bs == bUsed) = showParen (p > 10) $ eShow es
        | otherwise                      = showParen (p > 10)
                                         $ showString "overlay ("
                                         . veShow (vs \\ used)
                                         . showString ") ("
                                         . eShow es
                                         . showString ")"
      where
        as = leftVertexList g
        bs = rightVertexList g
        vs = vertexList g
        es = edgeList g
        aUsed = Set.toAscList $ Set.fromAscList [ a | (a, _) <- edgeList g ]
        bUsed = Set.toAscList $ Set.fromAscList [ b | (b, _) <- edgeList (swap g) ]
        used  = map Left aUsed ++ map Right bUsed
        vShow [a] []  = showString "leftVertex "  . showsPrec 11 a
        vShow []  [b] = showString "rightVertex " . showsPrec 11 b
        vShow as  bs  = showString "vertices "    . showsPrec 11 as
                      . showString " " . showsPrec 11 bs
        eShow [(a, b)] = showString "edge " . showsPrec 11 a
                       . showString " " . showsPrec 11 b
        eShow es       = showString "edges " . showsPrec 11 es
        veShow xs      = vShow (lefts xs) (rights xs)

-- | Construct the /empty graph/.
--
-- @
-- 'isEmpty' empty           == True
-- 'leftAdjacencyMap' empty  == Map.'Map.empty'
-- 'rightAdjacencyMap' empty == Map.'Map.empty'
-- 'hasVertex' x empty       == False
-- @
empty :: AdjacencyMap a b
empty = BAM Map.empty Map.empty

-- | Construct the graph comprising /a single isolated vertex/ in the left part.
--
-- @
-- 'leftAdjacencyMap' (leftVertex x)  == Map.'Map.singleton' x Set.'Set.empty'
-- 'rightAdjacencyMap' (leftVertex x) == Map.'Map.empty'
-- 'hasLeftVertex' x (leftVertex y)   == (x == y)
-- 'hasRightVertex' x (leftVertex y)  == False
-- 'hasEdge' x y (leftVertex z)       == False
-- @
leftVertex :: a -> AdjacencyMap a b
leftVertex a = BAM (Map.singleton a Set.empty) Map.empty

-- | Construct the graph comprising /a single isolated vertex/ in the right part.
--
-- @
-- 'leftAdjacencyMap' (rightVertex x)  == Map.'Map.empty'
-- 'rightAdjacencyMap' (rightVertex x) == Map.'Map.singleton' x Set.'Set.empty'
-- 'hasLeftVertex' x (rightVertex y)   == False
-- 'hasRightVertex' x (rightVertex y)  == (x == y)
-- 'hasEdge' x y (rightVertex z)       == False
-- @
rightVertex :: b -> AdjacencyMap a b
rightVertex b = BAM Map.empty (Map.singleton b Set.empty)

-- | Construct the graph comprising /a single isolated vertex/.
--
-- @
-- vertex . Left  == 'leftVertex'
-- vertex . Right == 'rightVertex'
-- @
vertex :: Either a b -> AdjacencyMap a b
vertex (Left  a) = leftVertex a
vertex (Right b) = rightVertex b

-- | Construct the graph comprising /a single edge/.
--
-- @
-- edge x y                     == 'connect' ('leftVertex' x) ('rightVertex' y)
-- 'leftAdjacencyMap' (edge x y)  == Map.'Map.singleton' x (Set.'Set.singleton' y)
-- 'rightAdjacencyMap' (edge x y) == Map.'Map.singleton' y (Set.'Set.singleton' x)
-- 'hasEdge' x y (edge x y)       == True
-- 'hasEdge' 1 2 (edge 2 1)       == False
-- @
edge :: a -> b -> AdjacencyMap a b
edge a b =
    BAM (Map.singleton a (Set.singleton b)) (Map.singleton b (Set.singleton a))

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
-- @
overlay :: (Ord a, Ord b) => AdjacencyMap a b -> AdjacencyMap a b -> AdjacencyMap a b
overlay (BAM ab1 ba1) (BAM ab2 ba2) =
    BAM (Map.unionWith Set.union ab1 ab2) (Map.unionWith Set.union ba1 ba2)

-- | /Connect/ two graphs, filtering out the edges between vertices of the same
-- part. This is a commutative and associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
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
connect (BAM ab1 ba1) (BAM ab2 ba2) = BAM ab ba
  where
    a1 = Map.keysSet ab1
    a2 = Map.keysSet ab2
    b1 = Map.keysSet ba1
    b2 = Map.keysSet ba2
    ab = Map.unionsWith Set.union
        [ ab1, ab2, Map.fromSet (const b2) a1, Map.fromSet (const b1) a2 ]
    ba = Map.unionsWith Set.union
        [ ba1, ba2, Map.fromSet (const a2) b1, Map.fromSet (const a1) b2 ]

-- | Construct the graph comprising given lists of isolated vertices in each
-- part.
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
vertices as bs = BAM (Map.fromList [ (a, Set.empty) | a <- as ])
                     (Map.fromList [ (b, Set.empty) | b <- bs ])

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
edges es = BAM (Map.fromListWith Set.union [ (a, Set.singleton b) | (a, b) <- es ])
               (Map.fromListWith Set.union [ (b, Set.singleton a) | (a, b) <- es ])

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
overlays xs = BAM (Map.unionsWith Set.union (map leftAdjacencyMap  xs))
                  (Map.unionsWith Set.union (map rightAdjacencyMap xs))

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

-- | Swap the parts of a given graph.
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
swap (BAM ab ba) = BAM ba ab

-- | Construct a bipartite 'AdjacencyMap' from an "Algebra.Graph.AdjacencyMap",
-- adding any missing edges to make the graph undirected and filtering out the
-- edges within the same parts.
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
-- toBipartite . 'Algebra.Graph.AdjacencyMap.clique'                   == 'uncurry' 'biclique' . 'partitionEithers'
-- toBipartite . 'fromBipartite'            == 'id'
-- @
toBipartite :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AdjacencyMap a b
toBipartite g = BAM (Map.fromAscList [ (a, getRights vs) | (Left  a, vs) <- am ])
                    (Map.fromAscList [ (b, getLefts  vs) | (Right b, vs) <- am ])
  where
    getRights = Set.fromAscList . rights . Set.toAscList
    getLefts  = Set.fromAscList . lefts  . Set.toAscList
    am        = Map.toAscList $ AM.adjacencyMap $ AM.symmetricClosure g

-- | Construct a bipartite 'AdjacencyMap' from an "Algebra.Graph.AdjacencyMap",
-- where the two parts are identified by a separate function, adding any missing
-- edges to make the graph undirected and filtering out the edges within the
-- same parts.
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

-- | Construct an "Algebra.Graph.AdjacencyMap" from a bipartite 'AdjacencyMap'.
-- Complexity: /O(m * log(n))/.
--
-- @
-- fromBipartite 'empty'          == 'Algebra.Graph.AdjacencyMap.empty'
-- fromBipartite ('leftVertex' x) == 'Algebra.Graph.AdjacencyMap.vertex' (Left x)
-- fromBipartite ('edge' x y)     == 'Algebra.Graph.AdjacencyMap.edges' [(Left x, Right y), (Right y, Left x)]
-- 'toBipartite' . fromBipartite  == 'id'
-- @
fromBipartite :: (Ord a, Ord b) => AdjacencyMap a b -> AM.AdjacencyMap (Either a b)
fromBipartite (BAM ab ba) = AM.fromAdjacencySets $
    [ (Left  a, Set.mapMonotonic Right bs) | (a, bs) <- Map.toAscList ab ] ++
    [ (Right b, Set.mapMonotonic Left  as) | (b, as) <- Map.toAscList ba ]

-- | Construct an "Algebra.Graph.AdjacencyMap" from a bipartite 'AdjacencyMap'
-- given a way to inject vertices of the two parts into the resulting vertex
-- type.
-- Complexity: /O(m * log(n))/.
--
-- @
-- fromBipartiteWith Left Right             == 'fromBipartite'
-- fromBipartiteWith id id ('vertices' xs ys) == 'Algebra.Graph.AdjacencyMap.vertices' (xs ++ ys)
-- fromBipartiteWith id id . 'edges'          == 'Algebra.Graph.AdjacencyMap.symmetricClosure' . 'Algebra.Graph.AdjacencyMap.edges'
-- @
fromBipartiteWith :: Ord c => (a -> c) -> (b -> c) -> AdjacencyMap a b -> AM.AdjacencyMap c
fromBipartiteWith f g (BAM ab ba) = AM.fromAdjacencySets $
    [ (f a, Set.map g bs) | (a, bs) <- Map.toAscList ab ] ++
    [ (g b, Set.map f as) | (b, as) <- Map.toAscList ba ]

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                 == True
-- isEmpty ('overlay' 'empty' 'empty') == True
-- isEmpty ('vertex' x)            == False
-- isEmpty                       == (==) 'empty'
-- @
isEmpty :: AdjacencyMap a b -> Bool
isEmpty (BAM ab ba) = Map.null ab && Map.null ba

-- | Check if a graph contains a given vertex in the left part.
-- Complexity: /O(log(l))/ time.
--
-- @
-- hasLeftVertex x 'empty'           == False
-- hasLeftVertex x ('leftVertex' y)  == (x == y)
-- hasLeftVertex x ('rightVertex' y) == False
-- @
hasLeftVertex :: Ord a => a -> AdjacencyMap a b -> Bool
hasLeftVertex a (BAM ab _) = Map.member a ab

-- | Check if a graph contains a given vertex in the right part.
-- Complexity: /O(log(r))/ time.
--
-- @
-- hasRightVertex x 'empty'           == False
-- hasRightVertex x ('leftVertex' y)  == False
-- hasRightVertex x ('rightVertex' y) == (x == y)
-- @
hasRightVertex :: Ord b => b -> AdjacencyMap a b -> Bool
hasRightVertex b (BAM _ ba) = Map.member b ba

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex . Left  == 'hasLeftVertex'
-- hasVertex . Right == 'hasRightVertex'
-- @
hasVertex :: (Ord a, Ord b) => Either a b -> AdjacencyMap a b -> Bool
hasVertex (Left  a) = hasLeftVertex a
hasVertex (Right b) = hasRightVertex b

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
hasEdge a b (BAM ab _) = (Set.member b <$> Map.lookup a ab) == Just True

-- | The number of vertices in the left part of a graph.
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

-- | The number of vertices in the right part of a graph.
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
-- Complexity: /O(l)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount . 'edges'    == 'length' . 'nub'
-- @
edgeCount :: AdjacencyMap a b -> Int
edgeCount = Map.foldr ((+) . Set.size) 0 . leftAdjacencyMap

-- | The sorted list of vertices of the left part of a graph.
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

-- | The sorted list of vertices of the right part of a graph.
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

-- | The sorted list of vertices of a graph.
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
edgeList (BAM ab _) = [ (a, b) | (a, bs) <- Map.toAscList ab, b <- Set.toAscList bs ]

-- | The set of vertices of the left part of a graph.
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

-- | The set of vertices of the right part of a graph.
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

-- TODO: Check if implementing this via 'Set.mapMonotonic' would be faster.
-- | The set of vertices of a graph.
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

-- | The set of edges of a graph.
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

-- | The sorted /adjacency list/ of the left part of a graph.
-- Complexity: /O(n + m)/ time and memory.
--
-- @
-- leftAdjacencyList 'empty'            == []
-- leftAdjacencyList ('vertices' [] xs) == []
-- leftAdjacencyList ('vertices' xs []) == [(x, []) | x <- 'nub' ('sort' xs)]
-- leftAdjacencyList ('edge' x y)       == [(x, [y])]
-- leftAdjacencyList ('star' x ys)      == [(x, 'nub' ('sort' ys))]
-- @
leftAdjacencyList :: AdjacencyMap a b -> [(a, [b])]
leftAdjacencyList (BAM ab _) = fmap Set.toAscList <$> Map.toAscList ab

-- | The sorted /adjacency list/ of the right part of a graph.
-- Complexity: /O(n + m)/ time and memory.
--
-- @
-- rightAdjacencyList 'empty'            == []
-- rightAdjacencyList ('vertices' [] xs) == [(x, []) | x <- 'nub' ('sort' xs)]
-- rightAdjacencyList ('vertices' xs []) == []
-- rightAdjacencyList ('edge' x y)       == [(y, [x])]
-- rightAdjacencyList ('star' x ys)      == [(y, [x])  | y <- 'nub' ('sort' ys)]
-- @
rightAdjacencyList :: AdjacencyMap a b -> [(b, [a])]
rightAdjacencyList (BAM _ ba) = fmap Set.toAscList <$> Map.toAscList ba

-- | A list of values of two alternating types. The first type argument denotes
-- the type of the value at the head.
--
-- With the @OverloadedLists@ extension it is possible to use the standard list
-- notation to construct a 'List' where the two types coincide, for example:
--
-- @
-- [1, 2, 3, 4, 5] :: List Int Int
-- @
--
-- We make use of this shorthand notation in the examples below.
data List a b = Nil | Cons a (List b a) deriving (Eq, Generic, Ord, Show)

instance IsList (List a a) where
    type Item (List a a) = a

    fromList = foldr Cons Nil

    toList Nil         = []
    toList (Cons a as) = a : toList as

-- | Construct a 'List' of even length from a list of pairs.
--
-- @
-- evenList []                 == 'Nil'
-- evenList [(1,2), (3,4)]     == [1, 2, 3, 4] :: 'List' Int Int
-- evenList [(1,\'a\'), (2,\'b\')] == 'Cons' 1 ('Cons' \'a\' ('Cons' 2 ('Cons' \'b\' 'Nil')))
-- @
evenList :: [(a, b)] -> List a b
evenList = foldr (\(a, b) -> Cons a . Cons b) Nil

-- | Construct a 'List' of odd length given the first element and a list of pairs.
--
-- @
-- oddList 1 []                 == 'Cons' 1 'Nil'
-- oddList 1 [(2,3), (4,5)]     == [1, 2, 3, 4, 5] :: 'List' Int Int
-- oddList 1 [(\'a\',2), (\'b\',3)] == 'Cons' 1 ('Cons' \'a\' ('Cons' 2 ('Cons' \'b\' ('Cons' 3 'Nil'))))
-- @
oddList :: a -> [(b, a)] -> List a b
oddList a = Cons a . evenList

-- | The /path/ on a 'List' of vertices.
-- Complexity: /O(L * log(L))/ time, where /L/ is the length of the given list.
--
-- @
-- path 'Nil'                   == 'empty'
-- path ('Cons' x 'Nil')          == 'leftVertex' x
-- path ('Cons' x ('Cons' y 'Nil')) == 'edge' x y
-- path [1, 2, 3, 4, 5]       == 'edges' [(1,2), (3,2), (3,4), (5,4)]
-- @
path :: (Ord a, Ord b) => List a b -> AdjacencyMap a b
path Nil          = empty
path (Cons a Nil) = leftVertex a
path abs          = edges (zip as bs ++ zip (drop 1 as) bs)
  where
    (as, bs) = split abs

    split :: List a b -> ([a], [b])
    split xs = case xs of
        Nil                 -> ([], [])
        Cons a Nil          -> ([a], [])
        Cons a (Cons b abs) -> (a : as, b : bs) where (as, bs) = split abs

-- | The /circuit/ on a list of pairs of vertices.
-- Complexity: /O(L * log(L))/ time, where L is the length of the given list.
--
-- @
-- circuit []                    == 'empty'
-- circuit [(x,y)]               == 'edge' x y
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

-- | The /star/ formed by a center vertex connected to a list of leaves.
-- Complexity: /O(L * log(L))/ time, where /L/ is the length of the given list.
--
-- @
-- star x []    == 'leftVertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('leftVertex' x) ('vertices' [] ys)
-- @
star :: (Ord a, Ord b) => a -> [b] -> AdjacencyMap a b
star x ys = connect (leftVertex x) (vertices [] ys)

-- | The /stars/ formed by overlaying a list of 'star's.
-- Complexity: /O(L * log(L))/ time, where /L/ is the total size of the input.
--
-- @
-- stars []                      == 'empty'
-- stars [(x, [])]               == 'leftVertex' x
-- stars [(x, [y])]              == 'edge' x y
-- stars [(x, ys)]               == 'star' x ys
-- stars                         == 'overlays' . 'map' ('uncurry' 'star')
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: (Ord a, Ord b) => [(a, [b])] -> AdjacencyMap a b
stars = overlays . map (uncurry star)

-- | Remove a vertex from the left part of a given graph.
-- Complexity: /O(r * log(l))/ time.
--
-- @
-- removeLeftVertex x ('leftVertex' x)       == 'empty'
-- removeLeftVertex 1 ('leftVertex' 2)       == 'leftVertex' 2
-- removeLeftVertex x ('rightVertex' y)      == 'rightVertex' y
-- removeLeftVertex x ('edge' x y)           == 'rightVertex' y
-- removeLeftVertex x . removeLeftVertex x == removeLeftVertex x
-- @
removeLeftVertex :: Ord a => a -> AdjacencyMap a b -> AdjacencyMap a b
removeLeftVertex a (BAM ab ba) = BAM (Map.delete a ab) (Map.map (Set.delete a) ba)

-- | Remove a vertex from the right part of a given graph.
-- Complexity: /O(l * log(r))/ time.
--
-- @
-- removeRightVertex x ('rightVertex' x)       == 'empty'
-- removeRightVertex 1 ('rightVertex' 2)       == 'rightVertex' 2
-- removeRightVertex x ('leftVertex' y)        == 'leftVertex' y
-- removeRightVertex y ('edge' x y)            == 'leftVertex' x
-- removeRightVertex x . removeRightVertex x == removeRightVertex x
-- @
removeRightVertex :: Ord b => b -> AdjacencyMap a b -> AdjacencyMap a b
removeRightVertex b (BAM ab ba) = BAM (Map.map (Set.delete b) ab) (Map.delete b ba)

-- | Remove an edge from a given graph.
-- Complexity: /O(log(l) + log(r))/ time.
--
-- @
-- removeEdge x y ('edge' x y)            == 'vertices' [x] [y]
-- removeEdge x y . removeEdge x y      == removeEdge x y
-- removeEdge x y . 'removeLeftVertex' x  == 'removeLeftVertex' x
-- removeEdge x y . 'removeRightVertex' y == 'removeRightVertex' y
-- @
removeEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> AdjacencyMap a b
removeEdge a b (BAM ab ba) =
    BAM (Map.adjust (Set.delete b) a ab) (Map.adjust (Set.delete a) b ba)

-- | Transform a graph by applying given functions to the vertices of each part.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- bimap f g 'empty'           == 'empty'
-- bimap f g . 'vertex'        == 'vertex' . Data.Bifunctor.'Data.Bifunctor.bimap' f g
-- bimap f g ('edge' x y)      == 'edge' (f x) (g y)
-- bimap 'id' 'id'               == 'id'
-- bimap f1 g1 . bimap f2 g2 == bimap (f1 . f2) (g1 . g2)
-- @
bimap :: (Ord a, Ord b, Ord c, Ord d) => (a -> c) -> (b -> d) -> AdjacencyMap a b -> AdjacencyMap c d
bimap f g (BAM ab ba) = BAM cd dc
  where
    cd = Map.map (Set.map g) $ Map.mapKeysWith Set.union f ab
    dc = Map.map (Set.map f) $ Map.mapKeysWith Set.union g ba

-- TODO: Add torus?
-- | Construct a /mesh/ graph from two lists of vertices.
-- Complexity: /O(L1 * L2 * log(L1 * L2))/ time, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh xs []           == 'empty'
-- mesh [] ys           == 'empty'
-- mesh [x] [y]         == 'leftVertex' (x,y)
-- mesh [1,1] [\'a\',\'b\'] == 'biclique' [(1,\'a\'), (1,\'b\')] [(1,\'a\'), (1,\'b\')]
-- mesh [1,2] [\'a\',\'b\'] == 'biclique' [(1,\'a\'), (2,\'b\')] [(1,\'b\'), (2,\'a\')]
-- @
mesh :: (Ord a, Ord b) => [a] -> [b] -> AdjacencyMap (a, b) (a, b)
mesh as bs = box (path $ fromList as) (path $ fromList bs)

-- | Compute the /Cartesian product/ of two graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'box' ('path' [0,1]) ('path' [\'a\',\'b\']) == 'edges' [ ((0,\'a\'), (0,\'b\'))
--                                            , ((0,\'a\'), (1,\'a\'))
--                                            , ((1,\'b\'), (0,\'b\'))
--                                            , ((1,\'b\'), (1,\'a\')) ]
-- @
-- Up to isomorphism between the resulting vertex types, this operation is
-- /commutative/, /associative/, /distributes/ over 'overlay', has singleton
-- graphs as /identities/ and /swapping identities/, and 'empty' as the
-- /annihilating zero/. Below @~~@ stands for equality up to an isomorphism,
-- e.g. @(x,@ @()) ~~ x@.
--
-- @
-- box x y                ~~ box y x
-- box x (box y z)        ~~ box (box x y) z
-- box x ('overlay' y z)    == 'overlay' (box x y) (box x z)
-- box x ('leftVertex' ())  ~~ x
-- box x ('rightVertex' ()) ~~ 'swap' x
-- box x 'empty'            ~~ 'empty'
-- 'vertexCount' (box x y)  == 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (box x y)  == 'vertexCount' x * 'edgeCount' y + 'edgeCount' x * 'vertexCount' y
-- @
box :: (Ord a, Ord b) => AdjacencyMap a a -> AdjacencyMap b b -> AdjacencyMap (a, b) (a, b)
box = boxWith (,) (,) (,) (,)

-- | Compute the generalised /Cartesian product/ of two graphs. The resulting
-- vertices are obtained using the given vertex combinators.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- See 'box' for some examples.
--
-- @
-- box == boxWith (,) (,) (,) (,)
-- @
boxWith :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)
        => (a -> c -> e) -> (b -> d -> e) -> (a -> d -> f) -> (b -> c -> f)
        -> AdjacencyMap a b -> AdjacencyMap c d -> AdjacencyMap e f
boxWith ac bd ad bc x y = toBipartite (AM.gmap combine ambox)
  where
    -- ambox :: AM.AdjacencyMap (Either a b, Either c d)
    ambox = AM.box (fromBipartite x) (fromBipartite y)

    -- combine :: (Either a b, Either c d) -> Either e f
    combine (Left  a, Left  c) = Left  (ac a c)
    combine (Left  a, Right d) = Right (ad a d)
    combine (Right b, Left  c) = Right (bc b c)
    combine (Right b, Right d) = Left  (bd b d)

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
