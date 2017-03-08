{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Data
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- The core data type for algebraic graphs and associated algorithms.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Data (
    -- * Algebraic data type for graphs
    Graph (..), fromGraph, foldg,

    -- * Graph transformation primitives
    induce, removeVertex, replaceVertex, mergeVertices, splitVertex, removeEdge,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge,

    -- * Graph composition
    box
  ) where

import Control.Monad
import Data.Foldable

import Algebra.Graph hiding (Graph)
import qualified Algebra.Graph.Classes as C
import qualified Algebra.Graph.HigherKinded.Classes as H
import Algebra.Graph.AdjacencyMap

-- | The 'Graph' datatype is a deep embedding of the core graph construction
-- primitives 'empty', 'vertex', 'overlay' and 'connect'. We define a
-- law-abiding 'Num' instance as a convenient notation when working with graphs:
--
-- @
-- 0           == Vertex 0
-- 1 + 2       == Overlay (Vertex 1) (Vertex 2)
-- 1 * 2       == Connect (Vertex 1) (Vertex 2)
-- 1 + 2 * 3   == Overlay (Vertex 1) (Connect (Vertex 2) (Vertex 3))
-- 1 * (2 + 3) == Connect (Vertex 1) (Overlay (Vertex 2) (Vertex 3))
-- @
data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving (Foldable, Functor, Show, Traversable)

instance C.Graph (Graph a) where
    type Vertex (Graph a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = Overlay
    connect = Connect

instance H.Graph Graph where
    empty   = Empty
    overlay = Overlay
    connect = Connect

instance Num a => Num (Graph a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

instance Ord a => Eq (Graph a) where
    x == y = fromGraph x == (fromGraph y :: AdjacencyMap a)

instance Applicative Graph where
    pure  = Vertex
    (<*>) = ap

instance Monad Graph where
    return  = Vertex
    g >>= f = foldg Empty f Overlay Connect g

-- | Check if the 'Graph' is empty. A convenient alias for `null`.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: Graph a -> Bool
isEmpty = null

-- | Check if the 'Graph' contains a given vertex. A convenient alias for `elem`.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex x ('removeVertex' x) == const False
-- @
hasVertex :: Eq a => a -> Graph a -> Bool
hasVertex = elem

-- | Fold a 'Graph' into the polymorphic graph expression. Semantically, this
-- operation acts as the identity, but allows to convert a 'Graph' to a
-- different data representation.
--
-- @
-- fromGraph g                 :: Graph a       == g
-- 'show' (fromGraph (1 * 2 + 3) :: Relation Int) == "graph [1,2,3] [(1,2)]"
-- @
fromGraph :: C.Graph g => Graph (Vertex g) -> g
fromGraph = foldg empty vertex overlay connect

-- | Generalised 'Graph' folding.
--
-- @
-- foldg []   return        (++) (++) == 'toList'
-- foldg 0    (const 1)     (+)  (+)  == 'length'
-- foldg True (const False) (&&) (&&) == 'isEmpty'
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg e v o c = go
  where
    go Empty         = e
    go (Vertex x)    = v x
    go (Overlay x y) = o (go x) (go y)
    go (Connect x y) = c (go x) (go y)

-- | Construct the /induced subgraph/ of a given 'Graph' by removing the
-- vertices that do not satisfy a given predicate.
--
-- @
-- induce (const True)  x    == x
-- induce (const False) x    == 'empty'
-- induce (/= x)             == 'removeVertex' x
-- induce p . induce q       == induce (\x -> p x && q x)
-- isSubgraph (induce p x) x == True
-- @
induce :: (a -> Bool) -> Graph a -> Graph a
induce p g = g >>= \v -> if p v then vertex v else empty

-- | Remove a vertex from a given 'Graph'.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Eq a => a -> Graph a -> Graph a
removeVertex v = induce (/= v)

-- | The function @replaceVertex x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Eq a => a -> a -> Graph a -> Graph a
replaceVertex u v = fmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate with a given vertex.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> Graph a -> Graph a
mergeVertices p v = fmap $ \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
--
-- @
-- splitVertex x []                   == 'removeVertex' x
-- splitVertex x [x]                  == id
-- splitVertex x [y]                  == 'replaceVertex' x y
-- splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex :: Eq a => a -> [a] -> Graph a -> Graph a
splitVertex v us g = g >>= \w -> if w == v then vertices us else vertex w

-- | Remove an edge from a given 'Graph'.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge s t g = piece st where (_, _, st) = smash s t g

-- | Check if the 'Graph' contains a given edge.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- @
hasEdge :: Eq a => a -> a -> Graph a -> Bool
hasEdge s t g = not $ intact st where (_, _, st) = smash s t g

data Piece a = Piece { piece :: Graph a, intact :: Bool }

breakIf :: Bool -> Piece a -> Piece a
breakIf True  _ = Piece Empty False
breakIf False x = x

instance C.Graph (Piece a) where
    type Vertex (Piece a) = a
    empty       = Piece Empty True
    vertex x    = Piece (Vertex x) True
    overlay x y = Piece (simple Overlay (piece x) (piece y)) (intact x && intact y)
    connect x y = Piece (simple Connect (piece x) (piece y)) (intact x && intact y)

simple :: (Graph a -> Graph a -> Graph a) -> Graph a -> Graph a -> Graph a
simple _ Empty x = x
simple _ x Empty = x
simple f x y     = f x y

type Pieces a = (Piece a, Piece a, Piece a)

smash :: Eq a => a -> a -> Graph a -> Pieces a
smash s t = foldg empty v overlay c
  where
    v x = (breakIf (x == s) $ vertex x, breakIf (x == t) $ vertex x, vertex x)
    c x@(sx, tx, stx) y@(sy, ty, sty)
        | intact sx || intact ty = connect x y
        | otherwise = (connect sx sy, connect tx ty, connect sx sty `overlay` connect stx ty)

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
box :: Graph a -> Graph b -> Graph (a, b)
box x y = overlays $ xs ++ ys
  where
    xs = map (\b -> fmap (,b) x) $ toList y
    ys = map (\a -> fmap (a,) y) $ toList x
