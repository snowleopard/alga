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
    removeEdge,

    -- * Graph properties
    hasEdge
  ) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad

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

instance Alternative Graph where
    empty = Empty
    (<|>) = Overlay

instance MonadPlus Graph where
    mzero = Empty
    mplus = Overlay

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
-- foldg []   return        (++) (++) == 'Data.Foldable.toList'
-- foldg 0    (const 1)     (+)  (+)  == 'Data.Foldable.length'
-- foldg True (const False) (&&) (&&) == 'Algebra.Graph.HigherKinded.Util.isEmpty'
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg e v o c = go
  where
    go Empty         = e
    go (Vertex x)    = v x
    go (Overlay x y) = o (go x) (go y)
    go (Connect x y) = c (go x) (go y)

-- | Remove an edge from a given 'Graph'.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'Algebra.Graph.HigherKinded.Util.removeVertex' x == 'Algebra.Graph.HigherKinded.Util.removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge s t g = piece st where (_, _, st) = smash s t g

-- | Check if a 'Graph' contains a given edge.
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
