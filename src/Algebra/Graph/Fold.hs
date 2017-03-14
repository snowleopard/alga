-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Fold
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Boehm-Berarducci encoding of algebraic graphs. It is used for generalised
-- graph folding and for the implementation of polymorphic graph construction
-- and transformation algorithms.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Fold (
    -- * Graph folding
    Fold, foldg,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, toSet, toIntSet,

    -- * Graph transformation
    transpose, simplify, gmap, replaceVertex, mergeVertices, bind, induce,
    removeVertex, splitVertex, removeEdge,

    -- * Graph composition
    box,

    -- * Graph construction
    mesh, torus, deBruijn,

    -- * Re-exporting standard functions
    toList
  ) where

import Data.Foldable
import Data.IntSet (IntSet)
import Data.Set (Set)

import Algebra.Graph.Base
import Algebra.Graph.Fold.Internal

import qualified Data.IntSet as IntSet
import qualified Data.Set    as Set

-- | The set of vertices of a given graph.
--
-- @
-- toSet 'empty'         == Set.empty
-- toSet ('vertex' x)    == Set.singleton x
-- toSet ('vertices' xs) == Set.fromList xs
-- toSet ('clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => Fold a -> Set a
toSet = foldg Set.empty Set.singleton Set.union Set.union

-- | The set of vertices of a given graph. Like 'toSet' but specicialised for
-- graphs with vertices of type 'Int'.
--
-- @
-- toIntSet 'empty'         == IntSet.empty
-- toIntSet ('vertex' x)    == IntSet.singleton x
-- toIntSet ('vertices' xs) == IntSet.fromList xs
-- toIntSet ('clique' xs)   == IntSet.fromList xs
-- @
toIntSet :: Fold Int -> IntSet
toIntSet = foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union

-- | Check if a graph is empty. A convenient alias for `null`.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('Algebra.Graph.Data.removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: Fold a -> Bool
isEmpty = null

-- | Check if a graph contains a given vertex. A convenient alias for `elem`.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Eq a => a -> Fold a -> Bool
hasVertex = elem

-- | Transpose a given graph.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- @
transpose :: Graph g => Fold (Vertex g) -> g
transpose = foldg empty vertex overlay (flip connect)

-- | Simplify a given graph. Semantically, this is the identity function, but
-- it simplifies a given polymorphic graph expression according to the laws of
-- the algebra. The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
-- Given an expression of size /n/, the function performs /O(n)/ graph
-- comparisons using the corresponding @Eq@ instance.
--
-- @
-- simplify x                        == x
-- 1 + 1 :: Graph Int                == Overlay (Vertex 1) (Vertex 1)
-- simplify (1 + 1) :: Graph Int     == Vertex 1
-- 1 * 1 * 1 :: Graph Int            == Connect (Connect (Vertex 1) (Vertex 1)) (Vertex 1)
-- simplify (1 * 1 * 1) :: Graph Int == Connect (Vertex 1) (Vertex 1)
-- @
simplify :: (Eq g, Graph g) => Fold (Vertex g) -> g
simplify = foldg empty vertex (simple overlay) (simple connect)

simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y

-- | The function @replaceVertex x y@ replaces vertex @x@ with vertex @y@ in a
-- given graph. If @y@ already exists, @x@ and @y@ will be merged.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: (Eq (Vertex g), Graph g) => Vertex g -> Vertex g -> Fold (Vertex g) -> g
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate with a given vertex.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: Graph g => (Vertex g -> Bool) -> Vertex g -> Fold (Vertex g) -> g
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
--
-- @
-- induce (const True)  x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'Algebra.Graph.isSubgraphOf' (induce p x) x == True
-- @
induce :: Graph g => (Vertex g -> Bool) -> Fold (Vertex g) -> g
induce p g = bind g $ \v -> if p v then vertex v else empty

-- | Remove a vertex from a given graph.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: (Eq (Vertex g), Graph g) => Vertex g -> Fold (Vertex g) -> g
removeVertex v = induce (/= v)

-- | Split a vertex into a list of vertices with the same connectivity.
--
-- @
-- splitVertex x []                   == 'removeVertex' x
-- splitVertex x [x]                  == id
-- splitVertex x [y]                  == 'replaceVertex' x y
-- splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex :: (Eq (Vertex g), Graph g) => Vertex g -> [Vertex g] -> Fold (Vertex g) -> g
splitVertex v vs g = bind g $ \u -> if u == v then vertices vs else vertex u

-- | Compute the /Cartesian product/ of graphs.
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
-- box x y             ~~ box y x
-- box x (box y z)     ~~ box (box x y) z
-- box x ('overlay' y z) == 'overlay' (box x y) (box x z)
-- box x ('vertex' ())   ~~ x
-- box x 'empty'         ~~ 'empty'
-- @
box :: (Graph g, Vertex g ~ (u, v)) => Fold u -> Fold v -> g
box x y = overlays $ xs ++ ys
  where
    xs = map (\b -> gmap (,b) x) $ toList y
    ys = map (\a -> gmap (a,) y) $ toList x

-- | Construct a /mesh graph/ from two lists of vertices.
--
-- @
-- mesh xs     []   == 'empty'
-- mesh []     ys   == 'empty'
-- mesh [x]    [y]  == 'vertex' (x, y)
-- mesh xs     ys   == 'box' ('path' xs) ('path' ys)
-- mesh [1..3] "ab" == 'edges' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\')), ((1,\'b\'),(2,\'b\')), ((2,\'a\'),(2,\'b\'))
--                           , ((2,\'a\'),(3,\'a\')), ((2,\'b\'),(3,\'b\')), ((3,\'a\'),(3,\'b\')) ]
-- @
mesh :: (Graph g, Vertex g ~ (u, v)) => [u] -> [v] -> g
mesh us vs = path us `box` path vs

-- | Construct a /torus graph/ from two lists of vertices.
--
-- @
-- torus xs     []   == 'empty'
-- torus []     ys   == 'empty'
-- torus [x]    [y]  == 'edge' (x, y) (x, y)
-- torus xs     ys   == 'box' ('circuit' xs) ('circuit' ys)
-- torus [1..2] "ab" == 'edges' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\')), ((1,\'b\'),(1,\'a\')), ((1,\'b\'),(2,\'b\'))
--                            , ((2,\'a\'),(1,\'a\')), ((2,\'a\'),(2,\'b\')), ((2,\'b\'),(1,\'b\')), ((2,\'b\'),(2,\'a\')) ]
-- @
torus :: (Graph g, Vertex g ~ (u, v)) => [u] -> [v] -> g
torus us vs = circuit us `box` circuit vs

-- | Remove an edge from a given graph.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'Algebra.Graph.HigherKinded.Util.removeVertex' x == 'Algebra.Graph.HigherKinded.Util.removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: (Eq (Vertex g), Graph g) => Vertex g -> Vertex g -> Fold (Vertex g) -> g
removeEdge s t g = piece st where (_, _, st) = smash s t g

-- | Check if a graph contains a given edge.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- @
hasEdge :: forall a. Eq a => a -> a -> Fold a -> Bool
hasEdge s t g = not $ intact st where (_, _, st :: Piece (Fold a)) = smash s t g

data Piece g = Piece { piece :: g, intact :: Bool, trivial :: Bool }

breakIf :: Graph g => Bool -> Piece g -> Piece g
breakIf True  _ = Piece empty False True
breakIf False x = x

instance Graph g => Graph (Piece g) where
    type Vertex (Piece g) = Vertex g
    empty       = Piece empty True True
    vertex x    = Piece (vertex x) True False
    overlay x y = Piece (nonTrivial overlay x y) (intact x && intact y) False
    connect x y = Piece (nonTrivial connect x y) (intact x && intact y) False

nonTrivial :: (g -> g -> g) -> Piece g -> Piece g -> g
nonTrivial f x y
    | trivial x = piece y
    | trivial y = piece x
    | otherwise = f (piece x) (piece y)

type Pieces a = (Piece a, Piece a, Piece a)

smash :: (Eq (Vertex g), Graph g) => Vertex g -> Vertex g -> Fold (Vertex g) -> Pieces g
smash s t = foldg empty v overlay c
  where
    v x = (breakIf (x == s) $ vertex x, breakIf (x == t) $ vertex x, vertex x)
    c x@(sx, tx, stx) y@(sy, ty, sty)
        | intact sx || intact ty = connect x y
        | otherwise = (connect sx sy, connect tx ty, connect sx sty `overlay` connect stx ty)

-- | Construct a /De Bruijn graph/ of given dimension and symbols of a given
-- alphabet.
--
-- @
-- deBruijn k []    == 'empty'
-- deBruijn 1 [0,1] == 'edges' [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]
-- deBruijn 2 "0"   == 'edge' "00" "00"
-- deBruijn 2 "01"  == 'edges' [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
--                           , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]
-- @
deBruijn :: (Graph g, Vertex g ~ [a]) => Int -> [a] -> g
deBruijn len alphabet = bind skeleton expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = edges    [        (Left s, Right s)   | s <- overlaps ]
    expand v = vertices [ either ([a] ++) (++ [a]) v | a <- alphabet ]
