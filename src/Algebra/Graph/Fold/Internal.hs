{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Fold.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of 'Fold'. The API is unstable
-- and unsafe. Where possible use non-internal module "Algebra.Graph.Fold"
-- instead.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Fold.Internal (
    -- * Graph folding
    Fold (..), foldg,

    -- * Graph transformation
    gmap, bind
  ) where

import Control.Applicative hiding (empty)
import Control.Monad

import Algebra.Graph.Class
import Algebra.Graph.AdjacencyMap (AdjacencyMap)

import qualified Algebra.Graph.HigherKinded.Class as H

-- | Boehm-Berarducci encoding of algebraic graphs.
-- The 'Show' instance is defined using basic graph construction primitives:
--
-- @
-- show ('empty'     :: Fold Int) == "empty"
-- show (1         :: Fold Int) == "vertex 1"
-- show (1 + 2     :: Fold Int) == "vertices [1,2]"
-- show (1 * 2     :: Fold Int) == "edge 1 2"
-- show (1 * 2 * 3 :: Fold Int) == "edges [(1,2),(1,3),(2,3)]"
-- show (1 * 2 + 3 :: Fold Int) == "graph [1,2,3] [(1,2)]"
-- @
newtype Fold a = Fold { runFold :: forall b. b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b }

instance (Ord a, Show a) => Show (Fold a) where
    show f = show (foldg empty vertex overlay connect f :: AdjacencyMap a)

-- | Generalised folding of polymorphic graph expressions.
--
-- @
-- foldg []   return        (++) (++) == 'toList'
-- foldg 0    (const 1)     (+)  (+)  == 'length'
-- foldg True (const False) (&&) (&&) == 'isEmpty'
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Fold a -> b
foldg e v o c g = runFold g e v o c

instance Graph (Fold a) where
    type Vertex (Fold a) = a
    empty       = Fold $ \e _ _ _ -> e
    vertex  x   = Fold $ \_ v _ _ -> v x
    overlay x y = Fold $ \e v o c -> foldg e v o c x `o` foldg e v o c y
    connect x y = Fold $ \e v o c -> foldg e v o c x `c` foldg e v o c y

instance Num a => Num (Fold a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- | Transform a given graph by applying a function to each of its vertices.
-- This is similar to 'fmap' but can be used with non-fully-parametric graphs.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: Graph g => (a -> Vertex g) -> Fold a -> g
gmap f = foldg empty (vertex . f) overlay connect

-- | Transform a given graph by substituting each of its vertices with a subgraph.
-- This is similar to Monad's bind '>>=' but can be used with non-fully-parametric
-- graphs.
--
-- @
-- bind 'empty' f         == 'empty'
-- bind ('vertex' x) f    == f x
-- bind ('edge' x y) f    == 'connect' (f x) (f y)
-- bind ('vertices' xs) f == 'overlays' ('map' f xs)
-- bind x (const 'empty') == 'empty'
-- bind x 'vertex'        == x
-- bind (bind x f) g    == bind x (\\y -> bind (f y) g)
-- @
bind :: Graph g => Fold a -> (a -> g) -> g
bind g f = foldg empty f overlay connect g

instance Functor Fold where
    fmap = gmap

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
    (>>=)  = bind

instance H.Graph Fold where
    connect = connect

instance Foldable Fold where
    foldMap f = foldg mempty f mappend mappend

instance Traversable Fold where
    traverse f = foldg (pure empty) (fmap vertex . f) (liftA2 overlay) (liftA2 connect)
