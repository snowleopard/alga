{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Data
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- The core data type for algebraic graphs.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Data (
    -- * Algebraic data type for graphs
    Graph (..), fold

  ) where

import Control.Monad

import qualified Algebra.Graph.Classes as C
import Algebra.Graph.Classes hiding (Graph)
import Algebra.Graph.AdjacencyMap

-- | The 'Graph' datatype is a deep embedding of the core graph construction
-- primitives 'empty', 'vertex', 'overlay' and 'connect'.
data Graph a = Empty
             | Vertex a
             | Overlay (Graph a) (Graph a)
             | Connect (Graph a) (Graph a)
             deriving (Show, Functor, Foldable, Traversable)

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
    return = vertex
    (>>=)  = flip foldMapGraph

fold :: C.Graph g => Graph (Vertex g) -> g
fold = foldMapGraph vertex

foldMapGraph :: C.Graph g => (a -> g) -> Graph a -> g
foldMapGraph _ Empty         = empty
foldMapGraph f (Vertex  x  ) = f x
foldMapGraph f (Overlay x y) = overlay (foldMapGraph f x) (foldMapGraph f y)
foldMapGraph f (Connect x y) = connect (foldMapGraph f x) (foldMapGraph f y)
