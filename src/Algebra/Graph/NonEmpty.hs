{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.NonEmpty
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the data type 'NonEmptyGraph' and associated algorithms.
--
-----------------------------------------------------------------------------
module Algebra.Graph.NonEmpty (
    -- * Algebraic data type for graphs
    NonEmptyGraph (..),

    -- * Basic graph construction primitives
    vertex, edge, overlay, connect, vertices1, edges1, overlays1, connects1,

    -- * Graph folding
    foldg1,

    -- * Relations on graphs
    isSubgraphOf, (===),

    -- * Graph properties
    size, hasVertex, hasEdge, vertexCount, edgeCount, edgeList, vertexSet,
    vertexIntSet, edgeSet,

    -- * Standard families of graphs
    path1, circuit1, clique1, biclique1, star, mesh1, torus1,

    -- * Graph transformation
    replaceVertex, mergeVertices, splitVertex, transpose, simplify,

    -- * Graph composition
    box
  ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq (NFData (..))
import Control.Monad.Compat
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe

import qualified Algebra.Graph                    as G
import qualified Algebra.Graph.AdjacencyMap       as AM
import qualified Algebra.Graph.Class              as C
import qualified Algebra.Graph.HigherKinded.Class as H
import qualified Algebra.Graph.IntAdjacencyMap    as IAM
import qualified Algebra.Graph.Relation           as R
import qualified Data.IntSet                      as IntSet
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set

data NonEmptyGraph a = Vertex a
                     | Overlay (NonEmptyGraph a) (NonEmptyGraph a)
                     | Connect (NonEmptyGraph a) (NonEmptyGraph a)
                     deriving (Foldable, Functor, Show, Traversable)

instance NFData a => NFData (NonEmptyGraph a) where
    rnf (Vertex  x  ) = rnf x
    rnf (Overlay x y) = rnf x `seq` rnf y
    rnf (Connect x y) = rnf x `seq` rnf y

instance C.ToGraph (NonEmptyGraph a) where
    type ToVertex (NonEmptyGraph a) = a
    toGraph = foldg1 C.vertex C.overlay C.connect

instance H.ToGraph NonEmptyGraph where
    toGraph = foldg1 H.vertex H.overlay H.connect

instance Ord a => Eq (NonEmptyGraph a) where
    x == y = C.toGraph x == (C.toGraph y :: AM.AdjacencyMap a)

instance Applicative NonEmptyGraph where
    pure  = Vertex
    (<*>) = ap

instance Monad NonEmptyGraph where
    return  = pure
    g >>= f = foldg1 f Overlay Connect g

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'hasVertex' 1 (vertex 2) == False
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- 'size'        (vertex x) == 1
-- @
vertex :: a -> NonEmptyGraph a
vertex = Vertex

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: a -> a -> NonEmptyGraph a
edge u v = connect (vertex u) (vertex v)

-- | /Overlay/ two graphs. An alias for the constructor 'Overlay'. This is an
-- idempotent, commutative and associative operation with the identity 'empty'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'size'        (overlay x y) == 'size' x        + 'size' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
overlay :: NonEmptyGraph a -> NonEmptyGraph a -> NonEmptyGraph a
overlay = Overlay

-- | /Connect/ two graphs. An alias for the constructor 'Connect'. This is an
-- associative operation with the identity 'empty', which distributes over the
-- overlay and obeys the decomposition axiom.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
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
-- 'size'        (connect x y) == 'size' x        + 'size' y
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- @
connect :: NonEmptyGraph a -> NonEmptyGraph a -> NonEmptyGraph a
connect = Connect

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices1 :: NonEmpty a -> NonEmptyGraph a
vertices1 = overlays1 . fmap vertex

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges1 :: NonEmpty (a, a) -> NonEmptyGraph a
edges1 = overlays1 . fmap (uncurry edge)

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays1 :: NonEmpty (NonEmptyGraph a) -> NonEmptyGraph a
overlays1 = foldr1 overlay

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects1 :: NonEmpty (NonEmptyGraph a) -> NonEmptyGraph a
connects1 = foldr1 connect

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, vertex, overlay and connect.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg 'empty' 'vertex'        'overlay' 'connect'        == id
-- foldg 'empty' 'vertex'        'overlay' (flip 'connect') == 'transpose'
-- foldg []    return        (++)    (++)           == 'Data.Foldable.toList'
-- foldg 0     (const 1)     (+)     (+)            == 'Data.Foldable.length'
-- foldg 1     (const 1)     (+)     (+)            == 'size'
-- foldg True  (const False) (&&)    (&&)           == 'isEmpty'
-- @
foldg1 :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> NonEmptyGraph a -> b
foldg1 v o c = go
  where
    go (Vertex  x  ) = v x
    go (Overlay x y) = o (go x) (go y)
    go (Connect x y) = c (go x) (go y)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf :: Ord a => NonEmptyGraph a -> NonEmptyGraph a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Structural equality on graph expressions.
-- Complexity: /O(s)/ time.
--
-- @
--     x === x         == True
--     x === x + 'empty' == False
-- x + y === x + y     == True
-- 1 + 2 === 2 + 1     == False
-- x + y === x * y     == False
-- @
(===) :: Eq a => NonEmptyGraph a -> NonEmptyGraph a -> Bool
(Vertex  x1   ) === (Vertex  x2   ) = x1 ==  x2
(Overlay x1 y1) === (Overlay x2 y2) = x1 === x2 && y1 === y2
(Connect x1 y1) === (Connect x2 y2) = x1 === x2 && y1 === y2
_               === _               = False

infix 4 ===

-- | The /size/ of a graph, i.e. the number of leaves of the expression
-- including 'empty' leaves.
-- Complexity: /O(s)/ time.
--
-- @
-- size 'empty'         == 1
-- size ('vertex' x)    == 1
-- size ('overlay' x y) == size x + size y
-- size ('connect' x y) == size x + size y
-- size x             >= 1
-- size x             >= 'vertexCount' x
-- @
size :: NonEmptyGraph a -> Int
size = foldg1 (const 1) (+) (+)

-- | Check if a graph contains a given vertex. A convenient alias for `elem`.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Eq a => a -> NonEmptyGraph a -> Bool
hasVertex v = foldg1 (==v) (||) (||)

-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: forall a. Ord a => a -> a -> NonEmptyGraph a -> Bool
hasEdge u v g = H.hasEdge u v (H.toGraph g :: G.Graph a)

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: Ord a => NonEmptyGraph a -> Int
vertexCount = length . vertexNonEmptyList

-- | The number of edges in a graph.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: forall a. Ord a => NonEmptyGraph a -> Int
edgeCount g = AM.edgeCount (C.toGraph g :: AM.AdjacencyMap a)

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexNonEmptyList :: forall a. Ord a => NonEmptyGraph a -> NonEmpty a
vertexNonEmptyList g = NonEmpty.fromList $ H.vertexList (H.toGraph g :: G.Graph a)

-- | The sorted list of edges of a graph.
-- Complexity: /O(s + m * log(m))/ time and /O(m)/ memory. Note that the number of
-- edges /m/ of a graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- edgeList . 'transpose'    == 'Data.List.sort' . map 'Data.Tuple.swap' . edgeList
-- @
edgeList :: forall a. Ord a => NonEmptyGraph a -> [(a, a)]
edgeList g = AM.edgeList (C.toGraph g :: AM.AdjacencyMap a)

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- vertexSet . 'clique'   == Set.'Set.fromList'
-- @
vertexSet :: forall a. Ord a => NonEmptyGraph a -> Set.Set a
vertexSet g = AM.vertexSet (C.toGraph g :: AM.AdjacencyMap a)

-- | The set of vertices of a given graph. Like 'vertexSet' but specialised for
-- graphs with vertices of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexIntSet 'empty'      == IntSet.'IntSet.empty'
-- vertexIntSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexIntSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexIntSet :: NonEmptyGraph Int -> IntSet.IntSet
vertexIntSet g = IAM.vertexIntSet (C.toGraph g :: IAM.IntAdjacencyMap)

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Ord a => NonEmptyGraph a -> Set.Set (a, a)
edgeSet = R.edgeSet . C.toGraph

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path1 :: NonEmpty a -> NonEmptyGraph a
path1 (x :| []    ) = vertex x
path1 (x :| (y:ys)) = edges1 ((x, y) :| zip (y:ys) ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edges' [(x,y), (y,x)]
-- circuit . 'reverse' == 'transpose' . circuit
-- @
circuit1 :: NonEmpty a -> NonEmptyGraph a
circuit1 (x :| xs) = path1 (x :| xs ++ [x])

-- | The /clique/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique . 'reverse'  == 'transpose' . clique
-- @
clique1 :: NonEmpty a -> NonEmptyGraph a
clique1 = connects1 . fmap vertex

-- | The /biclique/ on a list of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique1 :: NonEmpty a -> NonEmpty a -> NonEmptyGraph a
biclique1 xs ys = connect (vertices1 xs) (vertices1 ys)

-- | The /star/ formed by a centre vertex and a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- @
star :: a -> [a] -> NonEmptyGraph a
star u []     = vertex u
star u (x:xs) = connect (vertex u) (vertices1 $ x :| xs)

-- | Construct a /mesh graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh xs     []   == 'empty'
-- mesh []     ys   == 'empty'
-- mesh [x]    [y]  == 'vertex' (x, y)
-- mesh xs     ys   == 'box' ('path' xs) ('path' ys)
-- mesh [1..3] "ab" == 'edges' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\')), ((1,\'b\'),(2,\'b\')), ((2,\'a\'),(2,\'b\'))
--                           , ((2,\'a\'),(3,\'a\')), ((2,\'b\'),(3,\'b\')), ((3,\'a\'),(3,\'b\')) ]
-- @
mesh1 :: NonEmpty a -> NonEmpty b -> NonEmptyGraph (a, b)
mesh1 xs ys = path1 xs `box` path1 ys

-- | Construct a /torus graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus xs    []   == 'empty'
-- torus []    ys   == 'empty'
-- torus [x]   [y]  == 'edge' (x, y) (x, y)
-- torus xs    ys   == 'box' ('circuit' xs) ('circuit' ys)
-- torus [1,2] "ab" == 'edges' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\')), ((1,\'b\'),(1,\'a\')), ((1,\'b\'),(2,\'b\'))
--                           , ((2,\'a\'),(1,\'a\')), ((2,\'a\'),(2,\'b\')), ((2,\'b\'),(1,\'b\')), ((2,\'b\'),(2,\'a\')) ]
-- @
torus1 :: NonEmpty a -> NonEmpty b -> NonEmptyGraph (a, b)
torus1 xs ys = circuit1 xs `box` circuit1 ys

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Eq a => a -> a -> NonEmptyGraph a -> NonEmptyGraph a
replaceVertex u v = fmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate with a given vertex.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> NonEmptyGraph a -> NonEmptyGraph a
mergeVertices p v = fmap $ \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
-- Complexity: /O(s + k * L)/ time, memory and size, where /k/ is the number of
-- occurrences of the vertex in the expression and /L/ is the length of the
-- given list.
--
-- @
-- splitVertex x []                  == 'removeVertex' x
-- splitVertex x [x]                 == id
-- splitVertex x [y]                 == 'replaceVertex' x y
-- splitVertex 1 [0,1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex :: Eq a => a -> NonEmpty a -> NonEmptyGraph a -> NonEmptyGraph a
splitVertex v us g = g >>= \w -> if w == v then vertices1 us else vertex w

-- | Transpose a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- transpose . 'path'      == 'path'    . 'reverse'
-- transpose . 'circuit'   == 'circuit' . 'reverse'
-- transpose . 'clique'    == 'clique'  . 'reverse'
-- transpose ('box' x y)   == 'box' (transpose x) (transpose y)
-- 'edgeList' . transpose  == 'Data.List.sort' . map 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: NonEmptyGraph a -> NonEmptyGraph a
transpose = foldg1 vertex overlay (flip connect)

-- | Simplify a graph expression. Semantically, this is the identity function,
-- but it simplifies a given expression according to the laws of the algebra.
-- The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
-- Complexity: the function performs /O(s)/ graph comparisons. It is guaranteed
-- that the size of the result does not exceed the size of the given expression.
--
-- @
-- simplify              == id
-- 'size' (simplify x)     <= 'size' x
-- simplify 'empty'       '===' 'empty'
-- simplify 1           '===' 1
-- simplify (1 + 1)     '===' 1
-- simplify (1 + 2 + 1) '===' 1 + 2
-- simplify (1 * 1 * 1) '===' 1 * 1
-- @
simplify :: Ord a => NonEmptyGraph a -> NonEmptyGraph a
simplify = foldg1 Vertex (simple Overlay) (simple Connect)

simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O(s1 * s2)/ time, memory and size, where /s1/ and /s2/ are the
-- sizes of the given graphs.
--
-- @
-- box ('path' [0,1]) ('path' "ab") == 'edges' [ ((0,\'a\'), (0,\'b\'))
--                                       , ((0,\'a\'), (1,\'a\'))
--                                       , ((0,\'b\'), (1,\'b\'))
--                                       , ((1,\'a\'), (1,\'b\')) ]
-- @
-- Up to an isomorphism between the resulting vertex types, this operation
-- is /commutative/, /associative/, /distributes/ over 'overlay', has singleton
-- graphs as /identities/ and 'empty' as the /annihilating zero/. Below @~~@
-- stands for the equality up to an isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y               ~~ box y x
-- box x (box y z)       ~~ box (box x y) z
-- box x ('overlay' y z)   == 'overlay' (box x y) (box x z)
-- box x ('vertex' ())     ~~ x
-- box x 'empty'           ~~ 'empty'
-- 'transpose'   (box x y) == box ('transpose' x) ('transpose' y)
-- 'vertexCount' (box x y) == 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (box x y) <= 'vertexCount' x * 'edgeCount' y + 'edgeCount' x * 'vertexCount' y
-- @
box :: NonEmptyGraph a -> NonEmptyGraph b -> NonEmptyGraph (a, b)
box x y = overlays1 xs `overlay` overlays1 ys
  where
    xs = fmap (\b -> fmap (,b) x) $ fromJust $ NonEmpty.nonEmpty $ toList y
    ys = fmap (\a -> fmap (a,) y) $ fromJust $ NonEmpty.nonEmpty $ toList x
