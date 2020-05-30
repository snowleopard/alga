-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph
-- Copyright  : (c) Andrey Mokhov 2016-2020
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the core data type 'Graph' and associated algorithms.
-- For graphs that are known to be /non-empty/ at compile time, see
-- "Algebra.Graph.NonEmpty". 'Graph' is an instance of type classes defined in
-- modules "Algebra.Graph.Class" and "Algebra.Graph.HigherKinded.Class", which
-- can be used for polymorphic graph construction and manipulation.
--
-----------------------------------------------------------------------------
module Algebra.Graph (
    -- * Algebraic data type for graphs
    Graph (..),

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Graph folding
    foldg, buildg,

    -- * Relations on graphs
    isSubgraphOf, (===),

    -- * Graph properties
    isEmpty, size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, edgeSet, adjacencyList,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, tree, forest, mesh, torus,
    deBruijn,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, splitVertex,
    transpose, induce, induceJust, simplify, sparsify, sparsifyKL,

    -- * Graph composition
    compose, box,

    -- * Context
    Context (..), context
    ) where

import Control.Applicative (Alternative)
import Control.DeepSeq
import Control.Monad (MonadPlus (..))
import Control.Monad.State (runState, get, put)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.String
import Data.Tree
import GHC.Generics

import Algebra.Graph.Internal

import qualified Algebra.Graph.AdjacencyMap    as AM
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import qualified Control.Applicative           as Ap
import qualified Data.Graph                    as KL
import qualified Data.IntSet                   as IntSet
import qualified Data.Set                      as Set
import qualified Data.Tree                     as Tree
import qualified GHC.Exts                      as Exts

{-| The 'Graph' data type is a deep embedding of the core graph construction
primitives 'empty', 'vertex', 'overlay' and 'connect'. We define a 'Num'
instance as a convenient notation for working with graphs:

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

    * 'connect' is associative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
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
'Graph' expression. For example, if @g@ is a 'Graph' then /n/, /m/ and /s/ can
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

Converting a 'Graph' to the corresponding 'AM.AdjacencyMap' takes /O(s + m * log(m))/
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

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'isSubgraphOf' x y ==> x <= y@

@'empty' <= x
x     <= x + y
x + y <= x * y@

Deforestation (fusion) is implemented for some functions in this module. This means
that when a function tagged as a \"good producer\" is composed with a \"good consumer\",
the intermediate structure will not be built.
-}
data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving (Show, Generic)

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

-- | 'fmap' is a good consumer and producer.
instance Functor Graph where
    fmap f g = g >>= (vertex . f)
    {-# INLINE fmap #-}

instance NFData a => NFData (Graph a) where
    rnf Empty         = ()
    rnf (Vertex  x  ) = rnf x
    rnf (Overlay x y) = rnf x `seq` rnf y
    rnf (Connect x y) = rnf x `seq` rnf y

-- | __Note:__ this does not satisfy the usual ring laws; see 'Graph' for more
-- details.
instance Num a => Num (Graph a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

instance IsString a => IsString (Graph a) where
    fromString = Vertex . fromString

-- | `==` is a good consumer of both arguments.
instance Ord a => Eq (Graph a) where
    (==) = eqR

-- | 'compare' is a good consumer of both arguments.
instance Ord a => Ord (Graph a) where
    compare = ordR

-- TODO: Find a more efficient equality check.
-- Check if two graphs are equal by converting them to their adjacency maps.
eqR :: Ord a => Graph a -> Graph a -> Bool
eqR x y = toAdjacencyMap x == toAdjacencyMap y
{-# INLINE [2] eqR #-}
{-# RULES "eqR/Int" eqR = eqIntR #-}

-- Like 'eqR' but specialised for graphs with vertices of type 'Int'.
eqIntR :: Graph Int -> Graph Int -> Bool
eqIntR x y = toAdjacencyIntMap x == toAdjacencyIntMap y
{-# INLINE eqIntR #-}

-- TODO: Find a more efficient comparison.
-- Compare two graphs by converting them to their adjacency maps.
ordR :: Ord a => Graph a -> Graph a -> Ordering
ordR x y = compare (toAdjacencyMap x) (toAdjacencyMap y)
{-# INLINE [2] ordR #-}
{-# RULES "ordR/Int" ordR = ordIntR #-}

-- Like 'ordR' but specialised for graphs with vertices of type 'Int'.
ordIntR :: Graph Int -> Graph Int -> Ordering
ordIntR x y = compare (toAdjacencyIntMap x) (toAdjacencyIntMap y)
{-# INLINE ordIntR #-}

-- TODO: It should be a good consumer of its second argument too.
-- | `<*>` is a good consumer of its first agument and producer.
instance Applicative Graph where
    pure    = Vertex
    f <*> x = buildg $ \e v o c ->
      foldg e (\w -> foldg e (v . w) o c x) o c f
    {-# INLINE (<*>) #-}

-- | `>>=` is a good consumer and producer.
instance Monad Graph where
    return = pure
    g >>= f  = buildg $ \e v o c ->
      foldg e (composeR (foldg e v o c) f) o c g
    {-# INLINE (>>=) #-}

instance Alternative Graph where
    empty = Empty
    (<|>) = Overlay

instance MonadPlus Graph where
    mzero = Empty
    mplus = Overlay

-- | Construct the /empty graph/. An alias for the constructor 'Empty'.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- 'size'        empty == 1
-- @
empty :: Graph a
empty = Empty
{-# INLINE empty #-}

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex y) == (x == y)
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- 'size'        (vertex x) == 1
-- @
vertex :: a -> Graph a
vertex = Vertex
{-# INLINE vertex #-}

-- | Construct the graph comprising /a single edge/.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: a -> a -> Graph a
edge x y = connect (vertex x) (vertex y)
{-# INLINE edge #-}

-- | /Overlay/ two graphs. An alias for the constructor 'Overlay'. This is a
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
overlay :: Graph a -> Graph a -> Graph a
overlay = Overlay
{-# INLINE overlay #-}

-- | /Connect/ two graphs. An alias for the constructor 'Connect'. This is an
-- associative operation with the identity 'empty', which distributes over
-- 'overlay' and obeys the decomposition axiom.
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
connect :: Graph a -> Graph a -> Graph a
connect = Connect
{-# INLINE connect #-}

-- TODO: Simplifiy the definition to `overlays . map vertex` while presreving
-- goodness properties (which is not trivial since overlays is only a good
-- consumer of lists and not of lists of graphs).
-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- Good consumer of lists and producer of graphs.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: [a] -> Graph a
vertices xs = buildg $ \e v o _ -> combineR e o v xs
{-# INLINE vertices #-}

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- Good consumer of lists and producer of graphs.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'edge' x y
-- edges             == 'overlays' . 'map' ('uncurry' 'edge')
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges :: [(a, a)] -> Graph a
edges xs = buildg $ \e v o c ->
  combineR e o (\e -> c (v (fst e)) (v (snd e))) xs
{-# INLINE edges #-}

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- Good consumer of lists and producer of graphs.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: [Graph a] -> Graph a
overlays xs = buildg $ \e v o c -> combineR e o (foldg e v o c) xs
{-# INLINE overlays #-}

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- Good consumer of lists and producer of graphs.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: [Graph a] -> Graph a
connects xs = buildg $ \e v o c -> combineR e c (foldg e v o c) xs
{-# INLINE connects #-}

-- Safe version of foldr with a map (the composition is optimized)
-- This is a good consumer of lists.
combineR :: c -> (c -> c -> c) -> (a -> c) -> [a] -> c
combineR e o f = fromMaybe e . foldr1Safe o . map f
{-# INLINE combineR #-}

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, vertex, overlay and connect.
-- Complexity: /O(s)/ applications of the given functions. As an example, the
-- complexity of 'size' is /O(s)/, since 'const' and '+' have constant costs.
--
-- Good consumer.
--
-- @
-- foldg 'empty' 'vertex'        'overlay' 'connect'        == id
-- foldg 'empty' 'vertex'        'overlay' ('flip' 'connect') == 'transpose'
-- foldg 1     ('const' 1)     (+)     (+)            == 'size'
-- foldg True  ('const' False) (&&)    (&&)           == 'isEmpty'
-- foldg False (== x)        (||)    (||)           == 'hasVertex' x
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg e v o c = go
  where
    go Empty         = e
    go (Vertex  x  ) = v x
    go (Overlay x y) = o (go x) (go y)
    go (Connect x y) = c (go x) (go y)
{-# INLINE [0] foldg #-}

{-# RULES
"foldg/Empty"   forall e v o c.
    foldg e v o c Empty = e
"foldg/Vertex"  forall e v o c x.
    foldg e v o c (Vertex x) = v x
"foldg/Overlay" forall e v o c x y.
    foldg e v o c (Overlay x y) = o (foldg e v o c x) (foldg e v o c y)
"foldg/Connect" forall e v o c x y.
    foldg e v o c (Connect x y) = c (foldg e v o c x) (foldg e v o c y)
 #-}

-- | Build a graph given an interpretation of the four graph construction
-- primitives 'empty', 'vertex', 'overlay' and 'connect', in this order. See
-- examples for further clarification.
--
-- Functions expressed with 'buildg' are good producers.
--
-- @
-- buildg f                                                   == f 'empty' 'vertex' 'overlay' 'connect'
-- buildg (\\e _ _ _ -> e)                                     == 'empty'
-- buildg (\\_ v _ _ -> v x)                                   == 'vertex' x
-- buildg (\\e v o c -> o ('foldg' e v o c x) ('foldg' e v o c y)) == 'overlay' x y
-- buildg (\\e v o c -> c ('foldg' e v o c x) ('foldg' e v o c y)) == 'connect' x y
-- buildg (\\e v o _ -> 'foldr' o e ('map' v xs))                  == 'vertices' xs
-- buildg (\\e v o c -> 'foldg' e v o ('flip' c) g)                == 'transpose' g
-- 'foldg' e v o c (buildg f)                                   == f e v o c
-- @
buildg :: (forall r. r -> (a -> r) -> (r -> r -> r) -> (r -> r -> r) -> r) -> Graph a
buildg f = f Empty Vertex Overlay Connect
{-# INLINE [1] buildg #-}

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- Good consumer of both arguments.
--
-- @
-- isSubgraphOf 'empty'         x             ==  True
-- isSubgraphOf ('vertex' x)    'empty'         ==  False
-- isSubgraphOf x             ('overlay' x y) ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: Ord a => Graph a -> Graph a -> Bool
isSubgraphOf x y = AM.isSubgraphOf (toAdjacencyMap x) (toAdjacencyMap y)
{-# INLINE [2] isSubgraphOf #-}
{-# RULES "isSubgraphOf/Int" isSubgraphOf = isSubgraphOfIntR #-}

-- Like 'isSubgraphOf' but specialised for graphs with vertices of type 'Int'.
isSubgraphOfIntR :: Graph Int -> Graph Int -> Bool
isSubgraphOfIntR x y = AIM.isSubgraphOf (toAdjacencyIntMap x) (toAdjacencyIntMap y)
{-# INLINE isSubgraphOfIntR #-}

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
(===) :: Eq a => Graph a -> Graph a -> Bool
Empty           === Empty           = True
(Vertex  x1   ) === (Vertex  x2   ) = x1 ==  x2
(Overlay x1 y1) === (Overlay x2 y2) = x1 === x2 && y1 === y2
(Connect x1 y1) === (Connect x2 y2) = x1 === x2 && y1 === y2
_               === _               = False
{-# SPECIALISE (===) :: Graph Int -> Graph Int -> Bool #-}

infix 4 ===

-- | Check if a graph is empty.
-- Complexity: /O(s)/ time.
--
-- Good consumer.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: Graph a -> Bool
isEmpty = foldg True (const False) (&&) (&&)
{-# INLINE isEmpty #-}

-- | The /size/ of a graph, i.e. the number of leaves of the expression
-- including 'empty' leaves.
-- Complexity: /O(s)/ time.
--
-- Good consumer.
--
-- @
-- size 'empty'         == 1
-- size ('vertex' x)    == 1
-- size ('overlay' x y) == size x + size y
-- size ('connect' x y) == size x + size y
-- size x             >= 1
-- size x             >= 'vertexCount' x
-- @
size :: Graph a -> Int
size = foldg 1 (const 1) (+) (+)
{-# INLINE size #-}

-- | Check if a graph contains a given vertex.
-- Complexity: /O(s)/ time.
--
-- Good consumer.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' y)       == (x == y)
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex x = foldg False (==x) (||) (||)
{-# INLINE hasVertex #-}
{-# SPECIALISE hasVertex :: Int -> Graph Int -> Bool #-}

{- Note [The implementation of hasEdge]

We fold a graph into a function of type Int -> Int where the Int stands for the
number of vertices of the specified edge that have been matched so far. The edge
belongs to the graph if we reach the number 2. Note that this algorithm can be
generalised to algebraic graphs of higher dimensions, e.g. we can similarly find
3-edges (triangles), 4-edges (tetrahedra) and k-edges in O(s) time.

The four graph constructors are interpreted as follows:

  * Empty       : the matching number is unchanged;
  * Vertex x    : if x matches the next vertex, the number is incremented;
  * Overlay x y : pick the best match in the two subexpressions;
  * Connect x y : match the subexpressions one after another.

Note that in the last two cases we can (and do) shortcircuit the computation as
soon as the edge is fully matched in one of the subexpressions.
-}
-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- Good consumer.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Eq a => a -> a -> Graph a -> Bool
hasEdge s t g = foldg id v o c g 0 == 2
  where
    v x 0   = if x == s then 1 else 0
    v x _   = if x == t then 2 else 1
    o x y a = case x a of
        0 -> y a
        1 -> if y a == 2 then 2 else 1
        _ -> 2 :: Int
    c x y a = case x a of { 2 -> 2; res -> y res }
{-# INLINE hasEdge #-}
{-# SPECIALISE hasEdge :: Int -> Int -> Graph Int -> Bool #-}

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- Good consumer.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: Ord a => Graph a -> Int
vertexCount = Set.size . vertexSet
{-# INLINE [2] vertexCount #-}
{-# RULES "vertexCount/Int" vertexCount = vertexIntCountR #-}

-- Like 'vertexCount' but specialised for graphs with vertices of type 'Int'.
vertexIntCountR :: Graph Int -> Int
vertexIntCountR = IntSet.size . vertexIntSetR
{-# INLINE vertexIntCountR #-}

-- | The number of edges in a graph.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- Good consumer.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => Graph a -> Int
edgeCount = AM.edgeCount . toAdjacencyMap
{-# INLINE [2] edgeCount #-}
{-# RULES "edgeCount/Int" edgeCount = edgeCountIntR #-}

-- Like 'edgeCount' but specialised for graphs with vertices of type 'Int'.
edgeCountIntR :: Graph Int -> Int
edgeCountIntR = AIM.edgeCount . toAdjacencyIntMap
{-# INLINE edgeCountIntR #-}

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- Good consumer of graphs and producer of lists.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Ord a => Graph a -> [a]
vertexList = Set.toAscList . vertexSet
{-# INLINE [2] vertexList #-}
{-# RULES "vertexList/Int" vertexList = vertexIntListR #-}

-- Like 'vertexList' but specialised for graphs with vertices of type 'Int'.
vertexIntListR :: Graph Int -> [Int]
vertexIntListR = IntSet.toList . vertexIntSetR
{-# INLINE vertexIntListR #-}

-- | The sorted list of edges of a graph.
-- Complexity: /O(s + m * log(m))/ time and /O(m)/ memory. Note that the number of
-- edges /m/ of a graph can be quadratic with respect to the expression size /s/.
--
-- Good consumer of graphs and producer of lists.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- edgeList . 'transpose'    == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . edgeList
-- @
edgeList :: Ord a => Graph a -> [(a, a)]
edgeList = AM.edgeList . toAdjacencyMap
{-# INLINE [2] edgeList #-}
{-# RULES "edgeList/Int" edgeList = edgeIntListR #-}

-- Like 'edgeList' but specialised for graphs with vertices of type 'Int'.
edgeIntListR :: Graph Int -> [(Int, Int)]
edgeIntListR = AIM.edgeList . toAdjacencyIntMap
{-# INLINE edgeIntListR #-}

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- Good consumer.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: Ord a => Graph a -> Set.Set a
vertexSet = foldg Set.empty Set.singleton Set.union Set.union
{-# INLINE vertexSet #-}

-- Like 'vertexSet' but specialised for graphs with vertices of type 'Int'.
vertexIntSetR :: Graph Int -> IntSet.IntSet
vertexIntSetR = foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union
{-# INLINE vertexIntSetR #-}

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- Good consumer.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Ord a => Graph a -> Set.Set (a, a)
edgeSet = AM.edgeSet . toAdjacencyMap
{-# INLINE [2] edgeSet #-}
{-# RULES "edgeSet/Int" edgeSet = edgeIntSetR #-}

-- Like 'edgeSet' but specialised for graphs with vertices of type 'Int'.
edgeIntSetR :: Graph Int -> Set.Set (Int,Int)
edgeIntSetR = AIM.edgeSet . toAdjacencyIntMap
{-# INLINE edgeIntSetR #-}

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and memory.
--
-- Good consumer.
--
-- @
-- adjacencyList 'empty'          == []
-- adjacencyList ('vertex' x)     == [(x, [])]
-- adjacencyList ('edge' 1 2)     == [(1, [2]), (2, [])]
-- adjacencyList ('star' 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]
-- 'stars' . adjacencyList        == id
-- @
adjacencyList :: Ord a => Graph a -> [(a, [a])]
adjacencyList = AM.adjacencyList . toAdjacencyMap
{-# INLINE adjacencyList #-}
{-# SPECIALISE adjacencyList :: Graph Int -> [(Int, [Int])] #-}

-- TODO: This is a very inefficient implementation. Find a way to construct an
-- adjacency map directly, without building intermediate representations for all
-- subgraphs.
-- Convert a graph to 'AM.AdjacencyMap'.
toAdjacencyMap :: Ord a => Graph a -> AM.AdjacencyMap a
toAdjacencyMap = foldg AM.empty AM.vertex AM.overlay AM.connect
{-# INLINE toAdjacencyMap #-}

-- Like @toAdjacencyMap@ but specialised for graphs with vertices of type 'Int'.
toAdjacencyIntMap :: Graph Int -> AIM.AdjacencyIntMap
toAdjacencyIntMap = foldg AIM.empty AIM.vertex AIM.overlay AIM.connect
{-# INLINE toAdjacencyIntMap #-}

-- TODO: Make path a good consumer of lists, that is, express it with foldr.
-- This is not straightforward if we want to preserve efficiency.
-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- Good producer.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path :: [a] -> Graph a
path xs = buildg $ \e v o c ->
  case xs of
    []     -> e
    [x]    -> v x
    (_:ys) -> foldg e v o c $ edges (zip xs ys)
{-# INLINE path #-}

-- TODO: Make circuit a good consumer of lists, that is, express it with foldr.
-- This is not straightforward if we want to preserve efficiency.
-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- Good producer.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edges' [(x,y), (y,x)]
-- circuit . 'reverse' == 'transpose' . circuit
-- @
circuit :: [a] -> Graph a
circuit xs = buildg $ \e v o c ->
  case xs of
    [] -> e
    (x:xs) -> foldg e v o c $ path $ [x] ++ xs ++ [x]
{-# INLINE circuit #-}

-- | The /clique/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- Good consumer of lists and producer of graphs.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique . 'reverse'  == 'transpose' . clique
-- @
clique :: [a] -> Graph a
clique xs = buildg $ \e v _ c -> combineR e c v xs
{-# INLINE clique #-}

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- Good consumer of both arguments and producer of graphs.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: [a] -> [a] -> Graph a
biclique xs ys = buildg $ \e v o c ->
  case foldr1Safe o (map v xs) of
    Nothing -> foldg e v o c $ vertices ys
    Just xs ->
      case foldr1Safe o (map v ys) of
        Nothing -> xs
        Just ys -> c xs ys
{-# INLINE biclique #-}

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- Good consumer of lists and good producer of graphs.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: a -> [a] -> Graph a
star x ys = buildg $ \_ v o c ->
  case foldr1Safe o (map v ys) of
    Nothing -> v x
    Just vertices  -> c (v x) vertices
{-# INLINE star #-}

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the total size of the
-- input.
--
-- Good consumer of lists and producer of graphs.
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
stars :: [(a, [a])] -> Graph a
stars xs = buildg $ \e v o c ->
  combineR e o (foldg e v o c . uncurry star) xs
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
tree :: Tree.Tree a -> Graph a
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
forest :: Tree.Forest a -> Graph a
forest = overlays . map tree

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
mesh :: [a] -> [b] -> Graph (a, b)
mesh []  _   = empty
mesh _   []  = empty
mesh [x] [y] = vertex (x, y)
mesh xs  ys  = stars $  [ ((a1, b1), [(a1, b2), (a2, b1)]) | (a1, a2) <- ipxs, (b1, b2) <- ipys ]
                     ++ [ ((lx,y1), [(lx,y2)]) | (y1,y2) <- ipys]
                     ++ [ ((x1,ly), [(x2,ly)]) | (x1,x2) <- ipxs]
  where
    lx = last xs
    ly = last ys
    ipxs = init (pairs xs)
    ipys = init (pairs ys)

-- | Construct a /torus graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus xs    []   == 'empty'
-- torus []    ys   == 'empty'
-- torus [x]   [y]  == 'edge' (x,y) (x,y)
-- torus xs    ys   == 'box' ('circuit' xs) ('circuit' ys)
-- torus [1,2] "ab" == 'edges' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\')), ((1,\'b\'),(1,\'a\')), ((1,\'b\'),(2,\'b\'))
--                           , ((2,\'a\'),(1,\'a\')), ((2,\'a\'),(2,\'b\')), ((2,\'b\'),(1,\'b\')), ((2,\'b\'),(2,\'a\')) ]
-- @
torus :: [a] -> [b] -> Graph (a, b)
torus xs ys = stars [ ((a1, b1), [(a1, b2), (a2, b1)]) | (a1, a2) <- pairs xs, (b1, b2) <- pairs ys ]

-- | Auxiliary function for 'mesh' and 'torus'
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs as@(x:xs) = zip as (xs ++ [x])

-- | Construct a /De Bruijn graph/ of a given non-negative dimension using symbols
-- from a given alphabet.
-- Complexity: /O(A^(D + 1))/ time, memory and size, where /A/ is the size of the
-- alphabet and /D/ is the dimension of the graph.
--
-- @
--           deBruijn 0 xs               == 'edge' [] []
-- n > 0 ==> deBruijn n []               == 'empty'
--           deBruijn 1 [0,1]            == 'edges' [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]
--           deBruijn 2 "0"              == 'edge' "00" "00"
--           deBruijn 2 "01"             == 'edges' [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
--                                                , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]
--           'transpose'   (deBruijn n xs) == 'fmap' 'reverse' $ deBruijn n xs
--           'vertexCount' (deBruijn n xs) == ('length' $ 'Data.List.nub' xs)^n
-- n > 0 ==> 'edgeCount'   (deBruijn n xs) == ('length' $ 'Data.List.nub' xs)^(n + 1)
-- @
deBruijn :: Int -> [a] -> Graph [a]
deBruijn 0   _        = edge [] []
deBruijn len alphabet = skeleton >>= expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = edges    [        (Left s, Right s)   | s <- overlaps ]
    expand v = vertices [ either ([a] ++) (++ [a]) v | a <- alphabet ]

-- | Remove a vertex from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- Good consumer and producer.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Eq a => a -> Graph a -> Graph a
removeVertex v = induce (/= v)
{-# SPECIALISE removeVertex :: Int -> Graph Int -> Graph Int #-}

-- | Remove an edge from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- 'size' (removeEdge x y z)         <= 3 * 'size' z
-- @
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge s t = filterContext s (/=s) (/=t)
{-# SPECIALISE removeEdge :: Int -> Int -> Graph Int -> Graph Int #-}

-- TODO: Export
-- Filter vertices in a subgraph context.
filterContext :: Eq a => a -> (a -> Bool) -> (a -> Bool) -> Graph a -> Graph a
filterContext s i o g = maybe g go $ context (==s) g
  where
    go (Context is os) = induce (/=s) g `overlay` transpose (star s (filter i is))
                                        `overlay` star            s (filter o os)
{-# SPECIALISE filterContext :: Int -> (Int -> Bool) -> (Int -> Bool) -> Graph Int -> Graph Int #-}

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- Good consumer and producer.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Eq a => a -> a -> Graph a -> Graph a
replaceVertex u v = fmap $ \w -> if w == u then v else w
{-# INLINE replaceVertex #-}
{-# SPECIALISE replaceVertex :: Int -> Int -> Graph Int -> Graph Int #-}

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- constant time.
--
-- Good consumer and producer.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> Graph a -> Graph a
mergeVertices p v = fmap $ \w -> if p w then v else w
{-# INLINE mergeVertices #-}

-- | Split a vertex into a list of vertices with the same connectivity.
-- Complexity: /O(s + k * L)/ time, memory and size, where /k/ is the number of
-- occurrences of the vertex in the expression and /L/ is the length of the
-- given list.
--
-- Good consumer of lists and producer of graphs.
--
-- @
-- splitVertex x []                  == 'removeVertex' x
-- splitVertex x [x]                 == id
-- splitVertex x [y]                 == 'replaceVertex' x y
-- splitVertex 1 [0,1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex :: Eq a => a -> [a] -> Graph a -> Graph a
splitVertex x us g = buildg $ \e v o c ->
  let gus = foldg e v o c (vertices us) in
  foldg e (\w -> if w == x then gus else v w) o c g
{-# INLINE splitVertex #-}
{-# SPECIALISE splitVertex :: Int -> [Int] -> Graph Int -> Graph Int #-}

-- | Transpose a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- Good consumer and producer.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- transpose ('box' x y)   == 'box' (transpose x) (transpose y)
-- 'edgeList' . transpose  == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: Graph a -> Graph a
transpose g = buildg $ \e v o c -> foldg e v o (flip c) g
{-# INLINE transpose #-}

-- TODO: Implement via 'induceJust' to reduce code duplication.
-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- constant time.
--
-- Good consumer and producer.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> Graph a -> Graph a
induce p g = buildg $ \e v o c -> fromMaybe e $
  foldg Nothing (\x -> if p x then Just (v x) else Nothing) (k o) (k c) g
  where
    k _ x        Nothing  = x -- Constant folding to get rid of Empty leaves
    k _ Nothing  y        = y
    k f (Just x) (Just y) = Just (f x y)
{-# INLINE induce #-}

-- | Construct the /induced subgraph/ of a given graph by removing the vertices
-- that are 'Nothing'.
-- Complexity: /O(s)/ time, memory and size.
--
-- Good consumer and producer.
--
-- @
-- induceJust ('vertex' 'Nothing')                               == 'empty'
-- induceJust ('edge' ('Just' x) 'Nothing')                        == 'vertex' x
-- induceJust . 'fmap' 'Just'                                    == 'id'
-- induceJust . 'fmap' (\\x -> if p x then 'Just' x else 'Nothing') == 'induce' p
-- @
induceJust :: Graph (Maybe a) -> Graph a
induceJust g = buildg $ \e v o c -> fromMaybe e $
  foldg Nothing (fmap v) (k o) (k c) g
  where
    k _ x        Nothing  = x -- Constant folding to get rid of Empty leaves
    k _ Nothing  y        = y
    k f (Just x) (Just y) = Just (f x y)
{-# INLINE induceJust #-}

-- NB: This is not a good producer since it requires an Eq instance on the
-- produced structure.
-- | Simplify a graph expression. Semantically, this is the identity function,
-- but it simplifies a given expression according to the laws of the algebra.
-- The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
-- Complexity: the function performs /O(s)/ graph comparisons. It is guaranteed
-- that the size of the result does not exceed the size of the given expression.
--
-- Good consumer.
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
simplify :: Ord a => Graph a -> Graph a
simplify = foldg Empty Vertex (simple Overlay) (simple Connect)
{-# INLINE simplify #-}
{-# SPECIALISE simplify :: Graph Int -> Graph Int #-}

simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y
{-# SPECIALISE simple :: (Int -> Int -> Int) -> Int -> Int -> Int #-}

-- | Left-to-right /relational composition/ of graphs: vertices @x@ and @z@ are
-- connected in the resulting graph if there is a vertex @y@, such that @x@ is
-- connected to @y@ in the first graph, and @y@ is connected to @z@ in the
-- second graph. There are no isolated vertices in the result. This operation is
-- associative, has 'empty' and single-'vertex' graphs as /annihilating zeroes/,
-- and distributes over 'overlay'.
-- Complexity: /O(n * m * log(n))/ time, /O(n + m)/ memory, and /O(m1 + m2)/
-- size, where /n/ and /m/ stand for the number of vertices and edges in the
-- resulting graph, while /m1/ and /m2/ are the number of edges in the original
-- graphs. Note that the number of edges in the resulting graph may be
-- quadratic, i.e. /m = O(m1 * m2)/, but the algebraic representation requires
-- only /O(m1 + m2)/ operations to list them.
--
-- Good consumer of both arguments and good producer.
--
-- @
-- compose 'empty'            x                == 'empty'
-- compose x                'empty'            == 'empty'
-- compose ('vertex' x)       y                == 'empty'
-- compose x                ('vertex' y)       == 'empty'
-- compose x                (compose y z)    == compose (compose x y) z
-- compose x                ('overlay' y z)    == 'overlay' (compose x y) (compose x z)
-- compose ('overlay' x y)    z                == 'overlay' (compose x z) (compose y z)
-- compose ('edge' x y)       ('edge' y z)       == 'edge' x z
-- compose ('path'    [1..5]) ('path'    [1..5]) == 'edges' [(1,3), (2,4), (3,5)]
-- compose ('circuit' [1..5]) ('circuit' [1..5]) == 'circuit' [1,3,5,2,4]
-- 'size' (compose x y)                        <= 'edgeCount' x + 'edgeCount' y + 1
-- @
compose :: Ord a => Graph a -> Graph a -> Graph a
compose x y = buildg $ \e v o c -> fromMaybe e $
  foldr1Safe o
    [ foldg e v o c (biclique xs ys)
    | ve <- Set.toList (AM.vertexSet mx `Set.union` AM.vertexSet my)
    , let xs = Set.toList (AM.postSet ve mx), not (null xs)
    , let ys = Set.toList (AM.postSet ve my), not (null ys) ]
  where
    mx = toAdjacencyMap (transpose x)
    my = toAdjacencyMap y
{-# INLINE compose #-}

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
-- Up to the isomorphism between the resulting vertex types, this operation
-- is /commutative/, /associative/, /distributes/ over 'overlay', has singleton
-- graphs as /identities/ and 'empty' as the /annihilating zero/. Below @~~@
-- stands for equality up to the isomorphism, e.g. @(x, ()) ~~ x@.
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
box :: Graph a -> Graph b -> Graph (a, b)
box x y = overlay (fx <*> y) (fy <*> x)
  where
    fx = foldg empty (vertex .      (,)) overlay overlay x
    fy = foldg empty (vertex . flip (,)) overlay overlay y

-- | /Sparsify/ a graph by adding intermediate 'Left' @Int@ vertices between the
-- original vertices (wrapping the latter in 'Right') such that the resulting
-- graph is /sparse/, i.e. contains only O(s) edges, but preserves the
-- reachability relation between the original vertices. Sparsification is useful
-- when working with dense graphs, as it can reduce the number of edges from
-- O(n^2) down to O(n) by replacing cliques, bicliques and similar densely
-- connected structures by sparse subgraphs built out of intermediate vertices.
-- Complexity: O(s) time, memory and size.
--
-- @
-- 'Data.List.sort' . 'Algebra.Graph.ToGraph.reachable' x       == 'Data.List.sort' . 'Data.Either.rights' . 'Algebra.Graph.ToGraph.reachable' ('Data.Either.Right' x) . sparsify
-- 'vertexCount' (sparsify x) <= 'vertexCount' x + 'size' x + 1
-- 'edgeCount'   (sparsify x) <= 3 * 'size' x
-- 'size'        (sparsify x) <= 3 * 'size' x
-- @
sparsify :: Graph a -> Graph (Either Int a)
sparsify graph = res
  where
    (res, end) = runState (foldg e v o c graph 0 end) 1
    e     s t  = return $ path   [Left s,          Left t]
    v x   s t  = return $ clique [Left s, Right x, Left t]
    o x y s t  = overlay <$> s `x` t <*> s `y` t
    c x y s t  = do
        m <- get
        put (m + 1)
        overlay <$> s `x` m <*> m `y` t

-- | Sparsify a graph whose vertices are integers in the range @[1..n]@, where
-- @n@ is the first argument of the function, producing an array-based graph
-- representation from "Data.Graph" (introduced by King and Launchbury, hence
-- the name of the function). In the resulting graph, vertices @[1..n]@
-- correspond to the original vertices, and all vertices greater than @n@ are
-- introduced by the sparsification procedure.
--
-- Complexity: /O(s)/ time and memory. Note that thanks to sparsification, the
-- resulting graph has a linear number of edges with respect to the size of the
-- original algebraic representation even though the latter can potentially
-- contain a quadratic /O(s^2)/ number of edges.
--
-- @
-- 'Data.List.sort' . 'Algebra.Graph.ToGraph.reachable' k                 == 'Data.List.sort' . 'filter' (<= n) . 'flip' 'Data.Graph.reachable' k . sparsifyKL n
-- 'length' ('Data.Graph.vertices' $ sparsifyKL n x) <= 'vertexCount' x + 'size' x + 1
-- 'length' ('Data.Graph.edges'    $ sparsifyKL n x) <= 3 * 'size' x
-- @
sparsifyKL :: Int -> Graph Int -> KL.Graph
sparsifyKL n graph = KL.buildG (1, next - 1) ((n + 1, n + 2) : Exts.toList (res :: List KL.Edge))
  where
    (res, next) = runState (foldg e v o c graph (n + 1) (n + 2)) (n + 3)
    e     _ _   = return $ Exts.fromList []
    v x   s t   = return $ Exts.fromList [(s,x), (x,t)]
    o x y s t   = (<>) <$> s `x` t <*> s `y` t
    c x y s t   = do
        m <- get
        put (m + 1)
        (\xs ys -> Exts.fromList [(s,m), (m,t)] <> xs <> ys) <$> s `x` m <*> m `y` t

{- Note [The rules of foldg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The rules for foldg work very similarly to GHC's mapFB rules; see a note below
this line: http://hackage.haskell.org/package/base/docs/src/GHC.Base.html#mapFB.

* All concerned expressions are inlined to allow the compiler to apply the main
  rule: "foldg/buildg".
  This rule states that the composition of a good producer (expressed via buildg)
  and a good consumer (expressed via foldg) can be fused to remove the construction
  of the intermediate structure.

* If this inlining is made blindlessly, it can lead to unneeded operations. They
  are optimized via the "foldg/id" rule.

* composeR is here to allow further optimization. As an high-order function, it
  benefit from inlining in the final phase.

* The "composeR/composeR" rule optimises compositions of multiple composeR's.
-}

composeR :: (b -> c) -> (a -> b) -> a -> c
composeR = (.)
{-# INLINE [1] composeR #-}

-- Rewrite rules for fusion.
{-# RULES
-- Fuse a foldg followed by a buildg.
"foldg/buildg" forall e v o c (g :: forall b. b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b).
    foldg e v o c (buildg g) = g e v o c

-- Fuse composeR's (from bind's definition).
"composeR/composeR" forall c f g.
    composeR (composeR c f) g = composeR c (f . g)

-- Rewrite identity (which can appear in the inlining of 'buildg') to a more efficient one.
"foldg/id"
    foldg Empty Vertex Overlay Connect = id
 #-}

-- 'Focus' on a specified subgraph.
focus :: (a -> Bool) -> Graph a -> Focus a
focus f = foldg emptyFocus (vertexFocus f) overlayFoci connectFoci
{-# INLINE focus #-}

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
-- Good consumer.
--
-- @
-- context ('const' False) x                   == Nothing
-- context (== 1)        ('edge' 1 2)          == Just ('Context' [   ] [2  ])
-- context (== 2)        ('edge' 1 2)          == Just ('Context' [1  ] [   ])
-- context ('const' True ) ('edge' 1 2)          == Just ('Context' [1  ] [2  ])
-- context (== 4)        (3 * 1 * 4 * 1 * 5) == Just ('Context' [3,1] [1,5])
-- @
context :: (a -> Bool) -> Graph a -> Maybe (Context a)
context p g | ok f      = Just $ Context (toList $ is f) (toList $ os f)
            | otherwise = Nothing
  where
    f = focus p g
{-# INLINE context #-}
