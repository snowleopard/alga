-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.HigherKinded.Util
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines graph transformation and composition functions for
-- higher-kinded graphs defined in "Algebra.Graph.HigherKinded".
--
-----------------------------------------------------------------------------
module Algebra.Graph.HigherKinded.Util (
    -- * Graph transformation primitives
    induce, removeVertex, replaceVertex, mergeVertices, splitVertex,

    -- * Graph properties
    isEmpty, hasVertex,

    -- * Graph composition
    box
  ) where

import Control.Monad
import Data.Foldable

import Algebra.Graph.HigherKinded.Classes

-- | Construct the /induced subgraph/ of a given 'Graph' by removing the
-- vertices that do not satisfy a given predicate.
--
-- @
-- induce (const True)  x    == x
-- induce (const False) x    == 'empty'
-- induce (/= x)             == 'removeVertex' x
-- induce p . induce q       == induce (\x -> p x && q x)
-- 'Algebra.Graph.HigherKinded.isSubgraph' (induce p x) x == True
-- @
induce :: Graph g => (a -> Bool) -> g a -> g a
induce = mfilter

-- | Remove a vertex from a given 'Graph'.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: (Eq a, Graph g) => a -> g a -> g a
removeVertex v = induce (/= v)

-- | The function @replaceVertex x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: (Eq a, Graph g) => a -> a -> g a -> g a
replaceVertex u v = fmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate with a given vertex.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (Eq a, Graph g) => (a -> Bool) -> a -> g a -> g a
mergeVertices p v = fmap $ \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
--
-- @
-- splitVertex x []                   == 'removeVertex' x
-- splitVertex x [x]                  == id
-- splitVertex x [y]                  == 'replaceVertex' x y
-- splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex :: (Eq a, Graph g) => a -> [a] -> g a -> g a
splitVertex v us g = g >>= \w -> if w == v then msum (map vertex us) else vertex w

-- | Check if a 'Graph' is empty. A convenient alias for `null`.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('Algebra.Graph.Data.removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: (Graph g, Foldable g) => g a -> Bool
isEmpty = null

-- | Check if a 'Graph' contains a given vertex. A convenient alias for `elem`.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex x ('removeVertex' x) == const False
-- @
hasVertex :: (Eq a, Graph g, Foldable g) => a -> g a -> Bool
hasVertex = elem

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
box :: (Graph g, Foldable g) => g a -> g b -> g (a, b)
box x y = msum $ xs ++ ys
  where
    xs = map (\b -> fmap (,b) x) $ toList y
    ys = map (\a -> fmap (a,) y) $ toList x
