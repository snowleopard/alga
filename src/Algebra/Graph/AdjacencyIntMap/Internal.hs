{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyIntMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe, and is exposed only for documentation. You should use the
-- non-internal module "Algebra.Graph.AdjacencyIntMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyIntMap.Internal (
    -- * Adjacency map implementation
    AdjacencyIntMap (..), consistent
  ) where

import Prelude ()
import Prelude.Compat hiding (null)

import Data.Monoid (getSum, Sum (..))
import Data.IntMap.Strict (IntMap, keysSet, fromSet)
import Data.IntSet (IntSet)
import Data.List
import GHC.Generics

import Control.DeepSeq (NFData (..))

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet

{-| The 'AdjacencyIntMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a 'Num' instance as a convenient notation for
working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

__Note:__ the 'Num' instance does not satisfy several "customary laws" of 'Num',
which dictate that 'fromInteger' @0@ and 'fromInteger' @1@ should act as
additive and multiplicative identities, and 'negate' as additive inverse.
Nevertheless, overloading 'fromInteger', '+' and '*' is very convenient when
working with algebraic graphs; we hope that in future Haskell's Prelude will
provide a more fine-grained class hierarchy for algebraic structures, which we
would be able to utilise without violating any laws.

The 'Show' instance is defined using basic graph construction primitives:

@show (empty     :: AdjacencyIntMap Int) == "empty"
show (1         :: AdjacencyIntMap Int) == "vertex 1"
show (1 + 2     :: AdjacencyIntMap Int) == "vertices [1,2]"
show (1 * 2     :: AdjacencyIntMap Int) == "edge 1 2"
show (1 * 2 * 3 :: AdjacencyIntMap Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: AdjacencyIntMap Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'Algebra.Graph.AdjacencyIntMap.overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'Algebra.Graph.AdjacencyIntMap.connect' is associative and has
    'Algebra.Graph.AdjacencyIntMap.empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'Algebra.Graph.AdjacencyIntMap.connect' distributes over
    'Algebra.Graph.AdjacencyIntMap.overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'Algebra.Graph.AdjacencyIntMap.connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'Algebra.Graph.AdjacencyIntMap.overlay' has
    'Algebra.Graph.AdjacencyIntMap.empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'Algebra.Graph.AdjacencyIntMap.connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.

The total order on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Here are a few examples:

@'Algebra.Graph.AdjacencyIntMap.vertex' 1 < 'Algebra.Graph.AdjacencyIntMap.vertex' 2
'Algebra.Graph.AdjacencyIntMap.vertex' 3 < 'Algebra.Graph.AdjacencyIntMap.edge' 1 2
'Algebra.Graph.AdjacencyIntMap.vertex' 1 < 'Algebra.Graph.AdjacencyIntMap.edge' 1 1
'Algebra.Graph.AdjacencyIntMap.edge' 1 1 < 'Algebra.Graph.AdjacencyIntMap.edge' 1 2
'Algebra.Graph.AdjacencyIntMap.edge' 1 2 < 'Algebra.Graph.AdjacencyIntMap.edge' 1 1 + 'Algebra.Graph.AdjacencyIntMap.edge' 2 2
'Algebra.Graph.AdjacencyIntMap.edge' 1 2 < 'Algebra.Graph.AdjacencyIntMap.edge' 1 3@

Note that the resulting order refines the 'Algebra.Graph.AdjacencyIntMap.isSubgraphOf'
relation and is compatible with 'Algebra.Graph.AdjacencyIntMap.overlay' and
'Algebra.Graph.AdjacencyIntMap.connect' operations:

@'Algebra.Graph.AdjacencyIntMap.isSubgraphOf' x y ==> x <= y@

@'Algebra.Graph.AdjacencyIntMap.empty' <= x
x     <= x + y
x + y <= x * y@
-}
newtype AdjacencyIntMap = AM {
    -- | The /adjacency map/ of a graph: each vertex is associated with a set of
    -- its direct successors. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- adjacencyIntMap 'empty'      == IntMap.'IntMap.empty'
    -- adjacencyIntMap ('vertex' x) == IntMap.'IntMap.singleton' x IntSet.'IntSet.empty'
    -- adjacencyIntMap ('Algebra.Graph.AdjacencyIntMap.edge' 1 1) == IntMap.'IntMap.singleton' 1 (IntSet.'IntSet.singleton' 1)
    -- adjacencyIntMap ('Algebra.Graph.AdjacencyIntMap.edge' 1 2) == IntMap.'IntMap.fromList' [(1,IntSet.'IntSet.singleton' 2), (2,IntSet.'IntSet.empty')]
    -- @
    adjacencyIntMap :: IntMap IntSet } deriving (Eq, Generic)

instance Show AdjacencyIntMap where
    showsPrec p (AM m)
        | null vs    = showString "empty"
        | null es    = showParen (p > 10) $ vshow vs
        | vs == used = showParen (p > 10) $ eshow es
        | otherwise  = showParen (p > 10) $
                           showString "overlay (" . vshow (vs \\ used) .
                           showString ") (" . eshow es . showString ")"
      where
        vs             = IntSet.toAscList (keysSet m)
        es             = internalEdgeList m
        vshow [x]      = showString "vertex "   . showsPrec 11 x
        vshow xs       = showString "vertices " . showsPrec 11 xs
        eshow [(x, y)] = showString "edge "     . showsPrec 11 x .
                         showString " "         . showsPrec 11 y
        eshow xs       = showString "edges "    . showsPrec 11 xs
        used           = IntSet.toAscList (referredToVertexSet m)

instance Ord AdjacencyIntMap where
    compare (AM x) (AM y) = mconcat
        [ compare (vNum x) (vNum y)
        , compare (vSet x) (vSet y)
        , compare (eNum x) (eNum y)
        , compare       x        y ]
      where
        vNum = IntMap.size
        vSet = IntMap.keysSet
        eNum = getSum . foldMap (Sum . IntSet.size)

-- | __Note:__ this does not satisfy the usual ring laws; see 'AdjacencyIntMap'
-- for more details.
instance Num AdjacencyIntMap where
    fromInteger x = AM $ IntMap.singleton (fromInteger x) IntSet.empty
    x + y  = AM $ IntMap.unionWith IntSet.union (adjacencyIntMap x) (adjacencyIntMap y)
    x * y  = AM $ IntMap.unionsWith IntSet.union [ adjacencyIntMap x, adjacencyIntMap y,
        fromSet (const . keysSet $ adjacencyIntMap y) (keysSet $ adjacencyIntMap x) ]
    signum = const (AM IntMap.empty)
    abs    = id
    negate = id

instance NFData AdjacencyIntMap where
    rnf (AM a) = rnf a

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent 'Algebra.Graph.AdjacencyIntMap.empty'         == True
-- consistent ('Algebra.Graph.AdjacencyIntMap.vertex' x)    == True
-- consistent ('Algebra.Graph.AdjacencyIntMap.overlay' x y) == True
-- consistent ('Algebra.Graph.AdjacencyIntMap.connect' x y) == True
-- consistent ('Algebra.Graph.AdjacencyIntMap.edge' x y)    == True
-- consistent ('Algebra.Graph.AdjacencyIntMap.edges' xs)    == True
-- consistent ('Algebra.Graph.AdjacencyIntMap.stars' xs)    == True
-- @
consistent :: AdjacencyIntMap -> Bool
consistent (AM m) = referredToVertexSet m `IntSet.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: IntMap IntSet -> IntSet
referredToVertexSet = IntSet.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: IntMap IntSet -> [(Int, Int)]
internalEdgeList m = [ (x, y) | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]
