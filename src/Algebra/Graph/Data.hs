{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Data
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- The core data type for algebraic graphs and basic transformation algorithms.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Data (
    -- * Algebraic data type for graphs
    Graph (..), fold, foldg, toList,

    -- * Basic graph construction and transformation
    induce, removeVertex, replaceVertex, mergeVertices, splitVertex, removeEdge,
    box
  ) where

import Control.Monad
import Prelude hiding (foldMap)

import Algebra.Graph hiding (Graph)
import qualified Algebra.Graph.Classes as C
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
             deriving (Show, Functor)

instance C.Graph (Graph a) where
    type Vertex (Graph a) = a
    empty   = Empty
    vertex  = Vertex
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
    x == y = fold x == (fold y :: AdjacencyMap a)

instance Applicative Graph where
    pure  = vertex
    (<*>) = ap

instance Monad Graph where
    return  = vertex
    g >>= f = foldg Empty f Overlay Connect g

-- | Fold a 'Graph' into the polymorphic graph expression. Semantically, this
-- operation acts as the identity, but allows to convert a 'Graph' to a
-- different data representation. __TODO__: get rid of leaky Show instances of
-- ToList and Relation.
--
-- > fold g       :: Graph a      == g
-- > fold (1 * 2) :: ToList Int   == ToList {toList = [1,2]}
-- > fold (1 * 2) :: Relation Int == Relation {domain = fromList [1,2], relation = fromList [(1,2)]}
fold :: C.Graph g => Graph (Vertex g) -> g
fold = foldg empty vertex overlay connect

-- | Generalised 'Graph' folding.
--
-- > foldg []   return       (++) (++) == toList
-- > foldg 0    (const 1)    (+)  (+)  == length . toList
-- > foldg True (const False) (&&) (&&) == null . toList
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg e v o c = go
  where
    go Empty         = e
    go (Vertex x)    = v x
    go (Overlay x y) = o (go x) (go y)
    go (Connect x y) = c (go x) (go y)

-- | Turn a 'Graph' into a list of its vertices by traversing the expression
-- from left to right.
--
-- @
-- toList 'empty'   == []
-- toList 1       == [1]
-- toList (1 * 1) == [1, 1]
-- toList (1 * 0) == [1, 0]
-- @
toList :: Graph a -> [a]
toList = foldg [] return (++) (++)

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

-- | Replace a vertex with another one in a given 'Graph'.
--
-- @
-- replaceVertex x x                     == id
-- replaceVertex x y ('vertex' x)          == 'vertex' y
-- replaceVertex x y . replaceVertex y z == replaceVertex x z
-- @
replaceVertex :: Eq a => a -> a -> Graph a -> Graph a
replaceVertex u v = fmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (a -> Bool) -> a -> Graph a -> Graph a
mergeVertices p u = fmap $ \v -> if p v then u else v

-- | Split a vertex into a list of vertices with the same connectivity.
--
-- @
-- splitVertex x []                   == 'removeVertex' x
-- splitVertex x [x]                  == id
-- splitVertex x [y]                  == 'replaceVertex' x y
-- splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex :: Eq a => a -> [a] -> Graph a -> Graph a
splitVertex v vs g = g >>= \u -> if u == v then vertices vs else vertex u

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
removeEdge = undefined

-- | Compute the /Cartesian product/ of graphs.
--
-- @
-- box ('path' [0,1]) ('path' "ab") == 'edges' [ ((0,\'a\'), (0,\'b\'))
--                                       , ((0,\'a\'), (1,\'a\'))
--                                       , ((0,\'b\'), (1,\'b\'))
--                                       , ((1,\'a\'), (1,\'b\')) ]
-- @
-- Up to an isomorphism between the resulting vertex types, this operation
-- is /commutative/, /associative/, and
-- has @()@ as the /identity/ and 'empty' as the /annihilating zero/. Below @~~@
-- stands for the equality up to an isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y         ~~ box y x
-- box x (box y z) ~~ box (box x y) z
-- box x ()        ~~ x
-- box x 'empty'     ~~ 'empty'
-- @
box :: Graph a -> Graph b -> Graph (a, b)
box x y = overlays $ xs ++ ys
  where
    xs = map (\b -> fmap (,b) x) $ toList y
    ys = map (\a -> fmap (a,) y) $ toList x
