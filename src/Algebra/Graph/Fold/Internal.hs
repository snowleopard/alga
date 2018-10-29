{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Fold.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module exposes the implementation of the Boehm-Berarducci encoding of
-- algebraic graph. The API is unstable and unsafe, and is exposed only
-- for documentation. You should use the non-internal module "Algebra.Graph.Fold"
-- instead.
-----------------------------------------------------------------------------
module Algebra.Graph.Fold.Internal (
    -- * Boehm-Berarducci encoding of algebraic graphs
    Fold (..),

    -- * Basic graph construction primitives
    empty, vertex, overlay, connect,

    -- * Graph folding
    foldg
  )where

import Prelude ()
import Prelude.Compat
import Control.DeepSeq (NFData (..))

import Control.Applicative (Alternative, liftA2)
import Control.Monad.Compat (MonadPlus (..), ap)

import qualified Control.Applicative as Ap

import qualified Algebra.Graph.AdjacencyMap as AM

{-| The 'Fold' data type is the Boehm-Berarducci encoding of the core graph
construction primitives 'empty', 'vertex', 'overlay' and 'connect'. We define a
'Num' instance as a convenient notation for working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show (empty     :: Fold Int) == "empty"
show (1         :: Fold Int) == "vertex 1"
show (1 + 2     :: Fold Int) == "vertices [1,2]"
show (1 * 2     :: Fold Int) == "edge 1 2"
show (1 * 2 * 3 :: Fold Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: Fold Int) == "overlay (vertex 3) (edge 1 2)"@

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
graph expression. For example, if g is a 'Fold' then /n/, /m/ and /s/ can be
computed as follows:

@n == 'vertexCount' g
m == 'edgeCount' g
s == 'size' g@

Note that 'size' is slightly different from the 'length' method of the
'Foldable' type class, as the latter does not count 'empty' leaves of the
expression:

@'length' 'empty'           == 0
'size'   'empty'           == 1
'length' ('vertex' x)      == 1
'size'   ('vertex' x)      == 1
'length' ('empty' + 'empty') == 0
'size'   ('empty' + 'empty') == 2@

The 'size' of any graph is positive, and the difference @('size' g - 'length' g)@
corresponds to the number of occurrences of 'empty' in an expression @g@.

Converting a 'Fold' to the corresponding 'AM.AdjacencyMap' takes /O(s + m * log(m))/
time and /O(s + m)/ memory. This is also the complexity of the graph equality test,
because it is currently implemented by converting graph expressions to canonical
representations based on adjacency maps.

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
-}
newtype Fold a = Fold { runFold :: forall b. b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b }

instance (Ord a, Show a) => Show (Fold a) where
    show = show . foldg AM.empty AM.vertex AM.overlay AM.connect

instance Ord a => Ord (Fold a) where
    compare x y = compare (toAdjacencyMap x) (toAdjacencyMap y)

instance Ord a => Eq (Fold a) where
    x == y = adjacencyMap x == adjacencyMap y
      where
        adjacencyMap = AM.adjacencyMap . toAdjacencyMap

instance NFData a => NFData (Fold a) where
    rnf = foldg () rnf seq seq

instance Num a => Num (Fold a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Functor Fold where
    fmap f = foldg empty (vertex . f) overlay connect

instance Applicative Fold where
    pure  = vertex
    (<*>) = ap

instance Alternative Fold where
    empty = empty
    (<|>) = overlay

instance MonadPlus Fold where
    mzero = empty
    mplus = overlay

instance Monad Fold where
    return = vertex
    g >>=f = foldg empty f overlay connect g

instance Foldable Fold where
    foldMap f = foldg mempty f mappend mappend

instance Traversable Fold where
    traverse f = foldg (pure empty) (fmap vertex . f) (liftA2 overlay) (liftA2 connect)

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
empty :: Fold a
empty = Fold $ \e _ _ _ -> e
{-# NOINLINE [1] empty #-}

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
vertex :: a -> Fold a
vertex x = Fold $ \_ v _ _ -> v x
{-# NOINLINE [1] vertex #-}

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
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
overlay :: Fold a -> Fold a -> Fold a
overlay x y = Fold $ \e v o c -> runFold x e v o c `o` runFold y e v o c
{-# NOINLINE [1] overlay #-}

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
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
connect :: Fold a -> Fold a -> Fold a
connect x y = Fold $ \e v o c -> runFold x e v o c `c` runFold y e v o c
{-# NOINLINE [1] connect #-}

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
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Fold a -> b
foldg e v o c g = runFold g e v o c

toAdjacencyMap :: Ord a => Fold a -> AM.AdjacencyMap a
toAdjacencyMap = foldg AM.empty AM.vertex AM.overlay AM.connect
