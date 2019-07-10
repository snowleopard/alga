{-# LANGUAGE DeriveGeneric, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Undirected.UndirectedGraph
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines and abstraction over the core data type 'Graph' and associated algorithms.
-- Undirected graphs satisfy all laws of the
-- 'Algebra.Graph.Class.Undirected' type class, including the commutativity
-- of 'connect'.
-- For graphs that are known to be /non-empty/ at compile time, see
-- "Algebra.Graph.NonEmpty". 'Graph' is an instance of type classes defined in
-- modules "Algebra.Graph.Class" and "Algebra.Graph.HigherKinded.Class", which
-- can be used for polymorphic graph construction and manipulation.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Undirected.UndirectedGraph (
    -- * Algebraic data type for graphs
    UndirectedGraph (..),

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Graph folding
    foldg,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, edgeSet, adjacencyList,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices,
    induce

    ) where

import Control.Applicative (Alternative)
import Control.DeepSeq
import Control.Monad (MonadPlus (..))
import Control.Monad.State (runState, get, put)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, maybe)
import Data.Semigroup ((<>))
import Data.Tree
import Data.Tuple
import Data.List (nubBy)
import GHC.Generics

import qualified Algebra.Graph                 as G
import Algebra.Graph.Internal 

import qualified Algebra.Graph.AdjacencyMap    as AM
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import qualified Control.Applicative           as Ap
import qualified Data.Graph                    as KL
import qualified Data.IntSet                   as IntSet
import qualified Data.Set                      as Set
import qualified Data.Tree                     as Tree
import qualified GHC.Exts                      as Exts

{-| The 'UndirectedGraph' data type is an abstraction over the 'Graph' data
   type and provides the same graph construction
primitives 'empty', 'vertex', 'overlay' and 'connect'. We define the same 'Num' 
as 'Graph' instance as a convenient notation for working with graphs:

    > 0           == Vertex 0
    > 1 + 2       == Overlay (Vertex 1) (Vertex 2)
    > 1 * 2       == Connect (Vertex 1) (Vertex 2)
    > 1 + 2 * 3   == Overlay (Vertex 1) (Connect (Vertex 2) (Vertex 3))
    > 1 * (2 + 3) == Connect (Vertex 1) (Overlay (Vertex 2) (Vertex 3))

__Note:__ the 'Num' instance does not satisfy several "customary laws" of 'Num',
which dictate that 'fromInteger' @0@ and 'fromInteger' @1@ should act as
additive and multiplicative identities, and 'negate' as additive inverse.
Nevertheless, overloading 'fromInteger', '+' and '*' is very convenient when
working with algebraic graphs; we hope that in future Haskell's Prelude will
provide a more fine-grained class hierarchy for algebraic structures, which we
would be able to utilise without violating any laws.

The 'Eq' instance is currently implemented using the 'AM.AdjacencyMap' as the
/canonical graph representation/ and satisfies all axioms of algebraic graphs:

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is associative, commutative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        >   x * y     == y * x
        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ will
denote the number of vertices in the graph, /m/ will denote the number of
edges in the graph, and /s/ will denote the /size/ of the corresponding
'Graph' expression. For example, if @g@ is an 'UndirectedGraph' then /n/, /m/ and /s/ can
be computed as follows:

@n == 'vertexCount' g
m == 'edgeCount' g
s == 'size' g@

Note that 'size' counts all leaves of the expression:

@'vertexCount' 'empty'           == 0
'size'        'empty'           == 1
'vertexCount' ('vertex' x)      == 1
'size'        ('vertex' x)      == 1
'vertexCount' ('empty' + 'empty') == 0
'size'        ('empty' + 'empty') == 2@

Converting a 'UndirectedGraph' to the corresponding 'AM.AdjacencyMap' takes /O(s + m * log(m))/
time and /O(s + m)/ memory. This is also the complexity of the graph equality
test, because it is currently implemented by converting graph expressions to
canonical representations based on adjacency maps.

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
'edge' 1 2 == 'edge' 2 1

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'isSubgraphOf' x y ==> x <= y@

@'empty' <= x
x     <= x + y
x + y <= x * y@
-}
newtype UndirectedGraph a = UndirectedGraph { graph :: G.Graph a }
             deriving (Generic, NFData)

instance Show a => Show (UndirectedGraph a) where
  show = show . graph

{- Note [Functions for rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module contains several functions whose only purpose is to guide GHC
rewrite rules. The names of all such functions are suffixed with "R" so that it
is easier to distinguish them from others.

Why do we need them?

These functions are annotated with carefully chosen GHC pragmas that control
inlining, which would be impossible or unreliable if we used standard functions
instead. For example, the function 'eqR' has the following annotations:

    NOINLINE [1] eqR
    RULES "eqIntR" eqR = eqIntR

This tells GHC to rewrite 'eqR' to faster 'eqIntR' if possible (if the types
match), and -- importantly -- not to inline 'eqR' too early, before the rewrite
rule had a chance to fire.

We could have written the following rule instead:

    RULES "eqIntR" (==) = eqIntR

But that would have to rely on appropriate inlining behaviour of (==) which is
not under our control. We therefore choose the safe and more explicit path of
creating our own intermediate functions for guiding rewrite rules when needed.
-}

instance Functor UndirectedGraph where
    fmap = fmapR

fmapR :: (a -> b) -> UndirectedGraph a -> UndirectedGraph b
fmapR f g = bindR g (vertex . f)
{-# INLINE fmapR #-}

-- | __Note:__ this does not satisfy the usual ring laws; see 'Graph' for more
-- details.
instance Num a => Num (UndirectedGraph a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Ord a => Eq (UndirectedGraph a) where
    (==) = eqR

instance Ord a => Ord (UndirectedGraph a) where
    compare = ordR

-- TODO: Find a more efficient equality check.
-- Check if two graphs are equal by converting them to their adjacency maps.
eqR :: Ord a => UndirectedGraph a -> UndirectedGraph a -> Bool
eqR x y = toAdjacencyMap x == toAdjacencyMap y
{-# NOINLINE [1] eqR #-}
{-# RULES "eqR/Int" eqR = eqIntR #-}

-- Like 'eqR' but specialised for graphs with vertices of type 'Int'.
eqIntR :: UndirectedGraph Int -> UndirectedGraph Int -> Bool
eqIntR x y = toAdjacencyIntMap x == toAdjacencyIntMap y

-- TODO: Find a more efficient comparison.
-- Compare two graphs by converting them to their adjacency maps.
ordR :: Ord a => UndirectedGraph a -> UndirectedGraph a -> Ordering
ordR x y = compare (toAdjacencyMap x) (toAdjacencyMap y)
{-# NOINLINE [1] ordR #-}
{-# RULES "ordR/Int" ordR = ordIntR #-}

-- Like 'ordR' but specialised for graphs with vertices of type 'Int'.
ordIntR :: UndirectedGraph Int -> UndirectedGraph Int -> Ordering
ordIntR x y = compare (toAdjacencyIntMap x) (toAdjacencyIntMap y)

instance Applicative UndirectedGraph where
    pure  = vertex
    (<*>) = apR

apR :: UndirectedGraph (a -> b) -> UndirectedGraph a -> UndirectedGraph b
apR f x = bindR f (<$> x)
{-# INLINE apR #-}

instance Monad UndirectedGraph where
    return = pure
    (>>=)  = bindR

bindR :: UndirectedGraph a -> (a -> UndirectedGraph b) -> UndirectedGraph b
bindR g f = foldg empty f overlay connect g
{-# INLINE [0] bindR #-}

instance Alternative UndirectedGraph where
    empty = empty
    (<|>) = overlay

instance MonadPlus UndirectedGraph where
    mzero = empty
    mplus = overlay

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- 'size'        empty == 1
-- @
empty :: UndirectedGraph a
empty = UndirectedGraph G.Empty
{-# INLINE empty #-}

-- | Construct the graph comprising /a single isolated vertex/. 
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- 'size'        (vertex x) == 1
-- @
vertex :: a -> UndirectedGraph a
vertex = UndirectedGraph . G.Vertex
{-# INLINE vertex #-}

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- edge x y               == 'edge' y x
-- edge x y               == 'edges' [(x,y), (y,x)]
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: a -> a -> UndirectedGraph a
edge x y = connect (vertex x) (vertex y)

-- | /Overlay/ two graphs. This is a
-- commutative, associative and idempotent operation with the identity 'empty'.
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
overlay :: UndirectedGraph a -> UndirectedGraph a -> UndirectedGraph a
overlay g1 g2 = UndirectedGraph (G.Overlay (graph g1) (graph g2))
{-# INLINE overlay #-}

-- | /Connect/ two graphs. This is a commutative and
-- associative operation with the identity 'empty', which distributes over
-- 'overlay' and obeys the decomposition axiom.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'connect' x y               == 'connect' y x
-- 'isEmpty'     (connect x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y) >= 'vertexCount' x
-- 'vertexCount' (connect x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'edgeCount' x
-- 'edgeCount'   (connect x y) >= 'edgeCount' y
-- 'edgeCount'   (connect x y) >= 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'vertexCount' x * 'vertexCount' y `div` 2
-- 'size'        (connect x y) == 'size' x        + 'size' y
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- @
connect :: UndirectedGraph a -> UndirectedGraph a -> UndirectedGraph a
connect g1 g2 = UndirectedGraph (G.Connect (graph g1) (graph g2))
{-# INLINE connect #-}

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set . 'Set.fromList'
-- @
vertices :: [a] -> UndirectedGraph a
vertices = overlays . map vertex
{-# INLINE vertices #-}

-- TODO: Use a faster nubBy implementation with 'Data.Set'
-- | Construct the graph from a list of edges.
-- Complexity: /O(L^2)/ time, /O(L)/ memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'edge' x y
-- edges [(x,y), (y,x)] == 'edge' x y
-- @
edges :: Eq a => [(a, a)] -> UndirectedGraph a
edges = overlays . map (uncurry edge) . nubBy (\x y -> x == swap y)

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: [UndirectedGraph a] -> UndirectedGraph a
overlays = fromMaybe empty . foldr1Safe overlay
{-# INLINE [1] overlays #-}

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: [UndirectedGraph a] -> UndirectedGraph a
connects = fromMaybe empty . foldr1Safe connect
{-# INLINE [1] connects #-}

-- | Generalised 'UndirectedGraph' folding: recursively collapse an 'UndirectedGraph' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, vertex, overlay and connect.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg 'empty' 'vertex'        'overlay' 'connect'        == id
-- foldg 'empty' 'vertex'        'overlay' ('flip' 'connect') == id
-- foldg 1     ('const' 1)     (+)     (+)            == 'size'
-- foldg True  ('const' False) (&&)    (&&)           == 'isEmpty'
-- foldg False (== x)        (||)    (||)           == 'hasVertex' x
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> UndirectedGraph a -> b
foldg e v o c = go . graph
  where
    go G.Empty         = e
    go (G.Vertex  x  ) = v x
    go (G.Overlay x y) = o (go x) (go y)
    go (G.Connect x y) = c (go x) (go y)
{-# INLINE [0] foldg #-}

-- TODO: Add rule (?) 
    -- foldg e v o c (Connect x y) = c (foldg e v o c y) (foldg e v o c x)
{-# RULES
"foldg/Empty"   forall e v o c.
    foldg e v o c (UndirectedGraph G.Empty) = e
"foldg/Vertex"  forall e v o c x.
    foldg e v o c (UndirectedGraph (G.Vertex x)) = v x
"foldg/Overlay" forall e v o c x y.
    foldg e v o c (UndirectedGraph (G.Overlay x y)) = o (foldg e v o c (UndirectedGraph x)) (foldg e v o c (UndirectedGraph y))
"foldg/Connect" forall e v o c x y.
    foldg e v o c (UndirectedGraph (G.Connect x y)) = c (foldg e v o c (UndirectedGraph x)) (foldg e v o c (UndirectedGraph y))

"foldg/overlays" forall e v o c xs.
    foldg e v o c (overlays xs) = fromMaybe e (foldr (maybeF o . foldg e v o c) Nothing xs)
"foldg/connects" forall e v o c xs.
    foldg e v o c (connects xs) = fromMaybe e (foldr (maybeF c . foldg e v o c) Nothing xs)
 #-}

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- isSubgraphOf 'empty'         x             ==  True
-- isSubgraphOf ('vertex' x)    'empty'         ==  False
-- isSubgraphOf x             ('overlay' x y) ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  ==  True
-- isSubgraphOf ('edge' x y)    ('edge' y x)    ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: Ord a => UndirectedGraph a -> UndirectedGraph a -> Bool
isSubgraphOf x y = AM.isSubgraphOf (toAdjacencyMap x) (toAdjacencyMap y)
{-# NOINLINE [1] isSubgraphOf #-}
{-# RULES "isSubgraphOf/Int" isSubgraphOf = isSubgraphOfIntR #-}

-- Like 'isSubgraphOf' but specialised for graphs with vertices of type 'Int'.
isSubgraphOfIntR :: UndirectedGraph Int -> UndirectedGraph Int -> Bool
isSubgraphOfIntR x y = AIM.isSubgraphOf (toAdjacencyIntMap x) (toAdjacencyIntMap y)

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: UndirectedGraph a -> Bool
isEmpty = foldg True (const False) (&&) (&&)

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
size :: UndirectedGraph a -> Int
size = foldg 1 (const 1) (+) (+)

-- | Check if a graph contains a given vertex.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Eq a => a -> UndirectedGraph a -> Bool
hasVertex x = foldg False (==x) (||) (||)
{-# SPECIALISE hasVertex :: Int -> UndirectedGraph Int -> Bool #-}

-- TODO: Optimize somehow.
-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y ('edge' y x)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (min x y, max x y) . 'edgeList'
-- @
hasEdge :: Eq a => a -> a -> UndirectedGraph a -> Bool
hasEdge s t g = hit (graph g) == Edge || hit' (graph g) == Edge
  where
    hit G.Empty         = Miss
    hit (G.Vertex x   ) = if x == s then Tail else Miss
    hit (G.Overlay x y) = case hit x of
        Miss -> hit y
        Tail -> max Tail (hit y)
        Edge -> Edge
    hit (G.Connect x y) = case hit x of
        Miss -> hit y
        Tail -> if hasVertex t (UndirectedGraph y) then Edge else Tail
        Edge -> Edge
    hit' G.Empty         = Miss
    hit' (G.Vertex x   ) = if x == t then Tail else Miss
    hit' (G.Overlay x y) = case hit' x of
        Miss -> hit' y
        Tail -> max Tail (hit' y)
        Edge -> Edge
    hit' (G.Connect x y) = case hit' x of
        Miss -> hit' y
        Tail -> if hasVertex s (UndirectedGraph y) then Edge else Tail
        Edge -> Edge
{-# SPECIALISE hasEdge :: Int -> Int -> UndirectedGraph Int -> Bool #-}

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: Ord a => UndirectedGraph a -> Int
vertexCount = Set.size . vertexSet
{-# INLINE [1] vertexCount #-}
{-# RULES "vertexCount/Int" vertexCount = vertexIntCountR #-}

-- Like 'vertexCount' but specialised for graphs with vertices of type 'Int'.
vertexIntCountR :: UndirectedGraph Int -> Int
vertexIntCountR = IntSet.size . vertexIntSetR

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
edgeCount :: Ord a => UndirectedGraph a -> Int
edgeCount = AM.edgeCount . toAdjacencyMap
{-# INLINE [1] edgeCount #-}
{-# RULES "edgeCount/Int" edgeCount = edgeCountIntR #-}

-- Like 'edgeCount' but specialised for graphs with vertices of type 'Int'.
edgeCountIntR :: UndirectedGraph Int -> Int
edgeCountIntR = AIM.edgeCount . toAdjacencyIntMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Ord a => UndirectedGraph a -> [a]
vertexList = Set.toAscList . vertexSet
{-# INLINE [1] vertexList #-}
{-# RULES "vertexList/Int" vertexList = vertexIntListR #-}

-- Like 'vertexList' but specialised for graphs with vertices of type 'Int'.
vertexIntListR :: UndirectedGraph Int -> [Int]
vertexIntListR = IntSet.toList . vertexIntSetR

-- | The sorted list of edges of a graph.
-- Complexity: /O(s + m * log(m))/ time and /O(m)/ memory. Note that the number of
-- edges /m/ of a graph can be quadratic with respect to the expression size /s/.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(min x y, max y x)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- @
edgeList :: Ord a => UndirectedGraph a -> [(a, a)]
edgeList = AM.edgeList . toAdjacencyMap
{-# INLINE [1] edgeList #-}
{-# RULES "edgeList/Int" edgeList = edgeIntListR #-}

-- Like 'edgeList' but specialised for graphs with vertices of type 'Int'.
edgeIntListR :: UndirectedGraph Int -> [(Int, Int)]
edgeIntListR = AIM.edgeList . toAdjacencyIntMap

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: Ord a => UndirectedGraph a -> Set.Set a
vertexSet = foldg Set.empty Set.singleton Set.union Set.union

-- Like 'vertexSet' but specialised for graphs with vertices of type 'Int'.
vertexIntSetR :: UndirectedGraph Int -> IntSet.IntSet
vertexIntSetR = foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (min x y, max x y)
-- @
edgeSet :: Ord a => UndirectedGraph a -> Set.Set (a, a)
edgeSet = AM.edgeSet . toAdjacencyMap
{-# INLINE [1] edgeSet #-}
{-# RULES "edgeSet/Int" edgeSet = edgeIntSetR #-}

-- Like 'edgeSet' but specialised for graphs with vertices of type 'Int'.
edgeIntSetR :: UndirectedGraph Int -> Set.Set (Int,Int)
edgeIntSetR = AIM.edgeSet . toAdjacencyIntMap

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'          == []
-- adjacencyList ('vertex' x)     == [(x, [])]
-- adjacencyList ('edge' 1 2)     == [(1, [2]), (2, [1])]
-- adjacencyList ('star' 2 [3,1]) == [(1, [2]), (2, [1,3]), (3, [2])]
-- 'stars' . adjacencyList        == id
-- @
adjacencyList :: Ord a => UndirectedGraph a -> [(a, [a])]
adjacencyList = AM.adjacencyList . toAdjacencyMap
{-# SPECIALISE adjacencyList :: UndirectedGraph Int -> [(Int, [Int])] #-}

-- TODO: This is a very inefficient implementation. Find a way to construct an
-- adjacency map directly, without building intermediate representations for all
-- subgraphs.
-- Convert a graph to 'AM.AdjacencyMap'.
toAdjacencyMap :: Ord a => UndirectedGraph a -> AM.AdjacencyMap a
toAdjacencyMap = foldg AM.empty AM.vertex AM.overlay AM.connect

-- Like @toAdjacencyMap@ but specialised for graphs with vertices of type 'Int'.
toAdjacencyIntMap :: UndirectedGraph Int -> AIM.AdjacencyIntMap
toAdjacencyIntMap = foldg AIM.empty AIM.vertex AIM.overlay AIM.connect

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == path 
-- @
path :: Eq a => [a] -> UndirectedGraph a
path xs = case xs of []     -> empty
                     [x]    -> vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edge' (x,y)
-- circuit . 'reverse' == circuit
-- @
circuit :: Eq a => [a] -> UndirectedGraph a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

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
-- clique . 'reverse'  == clique
-- @
clique :: [a] -> UndirectedGraph a
clique = connects . map vertex
{-# INLINE [1] clique #-}

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,x2), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: [a] -> [a] -> UndirectedGraph a
biclique xs [] = vertices xs
biclique [] ys = vertices ys
biclique xs ys = connect (vertices xs) (vertices ys)

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: a -> [a] -> UndirectedGraph a
star x [] = vertex x
star x ys = connect (vertex x) (vertices ys)
{-# INLINE star #-}

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the total size of the
-- input.
--
-- @
-- stars []                      == 'empty'
-- stars [(x, [])]               == 'vertex' x
-- stars [(x, [y])]              == 'edge' x y
-- stars [(x, ys)]               == 'star' x ys
-- stars                         == 'overlays' . 'map' ('uncurry' 'star')
-- stars . 'adjacencyList'         == id
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: [(a, [a])] -> UndirectedGraph a
stars = overlays . map (uncurry star)
{-# INLINE stars #-}

-- | The /tree graph/ constructed from a given 'Tree.Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree (i.e. the number of vertices in the tree).
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Tree.Tree a -> UndirectedGraph a
tree (Node x []) = vertex x
tree (Node x f ) = star x (map rootLabel f)
         `overlay` forest (filter (not . null . subForest) f)

-- | The /forest graph/ constructed from a given 'Tree.Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest (i.e. the number of vertices in the forest).
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . 'map' 'tree'
-- @
forest :: Tree.Forest a -> UndirectedGraph a
forest = overlays . map tree

-- | Remove a vertex from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Eq a => a -> UndirectedGraph a -> UndirectedGraph a
removeVertex v = induce (/= v)
{-# SPECIALISE removeVertex :: Int -> UndirectedGraph Int ->
  UndirectedGraph Int #-}

-- | Remove an edge from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y                  == removeEdge y x
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Eq a => a -> a -> UndirectedGraph a -> UndirectedGraph a
removeEdge s t = filterContext s (/=s) (/=t) . filterContext s (/=t) (/=s)
{-# SPECIALISE removeEdge :: Int -> Int -> UndirectedGraph Int ->
  UndirectedGraph Int #-}

-- TODO: Export
-- Filter vertices in a subgraph context.
filterContext :: Eq a => a -> (a -> Bool) -> (a -> Bool) -> UndirectedGraph a -> UndirectedGraph a
filterContext s i o g = maybe g go $ context (==s) g
  where
    go (Context is os) = induce (/=s) g `overlay` star s (filter i is)
                                        `overlay` star s (filter o os)
{-# SPECIALISE filterContext :: Int -> (Int -> Bool) -> (Int -> Bool) -> UndirectedGraph Int -> UndirectedGraph Int #-}

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Eq a => a -> a -> UndirectedGraph a -> UndirectedGraph a
replaceVertex u v = fmap $ \w -> if w == u then v else w
{-# SPECIALISE replaceVertex :: Int -> Int -> UndirectedGraph Int -> UndirectedGraph Int #-}

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> UndirectedGraph a -> UndirectedGraph a
mergeVertices p v = fmap $ \w -> if p w then v else w

-- TODO: Implement via 'induceJust' to reduce code duplication.
-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> UndirectedGraph a -> UndirectedGraph a
induce p = foldg empty (\x -> if p x then vertex x else empty) (k overlay) (k connect) 
  where
    k _ x     empty = x -- Constant folding to get rid of Empty leaves
    k _ empty y     = y
    k f x     y     = f x y
{-# INLINE [1] induce #-}

-- 'Focus' on a specified subgraph.
focus :: (a -> Bool) -> UndirectedGraph a -> Focus a
focus f = foldg emptyFocus (vertexFocus f) overlayFoci connectFoci

-- | The 'Context' of a subgraph comprises its 'inputs' and 'outputs', i.e. all
-- the vertices that are connected to the subgraph's vertices. Note that inputs
-- and outputs can belong to the subgraph itself. In general, there are no
-- guarantees on the order of vertices in 'inputs' and 'outputs'; furthermore,
-- there may be repetitions.
data Context a = Context { inputs :: [a], outputs :: [a] }
    deriving (Eq, Show)

-- | Extract the 'Context' of a subgraph specified by a given predicate. Returns
-- @Nothing@ if the specified subgraph is empty.
--
-- @
-- context ('const' False) x                   == Nothing
-- context (== 1)        ('edge' 1 2)          == Just ('Context' [   ] [2  ])
-- context (== 2)        ('edge' 1 2)          == Just ('Context' [1  ] [   ])
-- context ('const' True ) ('edge' 1 2)          == Just ('Context' [1  ] [2  ])
-- context (== 4)        (3 * 1 * 4 * 1 * 5) == Just ('Context' [3,1] [1,5])
-- @
context :: (a -> Bool) -> UndirectedGraph a -> Maybe (Context a)
context p g | ok f      = Just $ Context (toList $ is f) (toList $ os f)
            | otherwise = Nothing
  where
    f = focus p g
