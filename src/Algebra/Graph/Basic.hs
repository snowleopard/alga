{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.Basic (Basic (..), foldBasic, Undirected (..)) where

import Algebra.Graph
import Algebra.Graph.Relation

data Basic a = Empty
             | Vertex a
             | Overlay (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)
             deriving (Show, Functor, Foldable, Traversable)

instance Graph (Basic a) where
    type Vertex (Basic a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = Overlay
    connect = Connect

instance Num a => Num (Basic a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

instance Ord a => Eq (Basic a) where
    x == y = toRelation x == toRelation y
      where
        toRelation :: Ord a => Basic a -> Relation a
        toRelation = foldBasic

foldBasic :: (Vertex g ~ a, Graph g) => Basic a -> g
foldBasic Empty         = empty
foldBasic (Vertex  x  ) = vertex x
foldBasic (Overlay x y) = overlay (foldBasic x) (foldBasic y)
foldBasic (Connect x y) = connect (foldBasic x) (foldBasic y)

newtype Undirected a = Undirected { graph :: Basic a } deriving (Num, Show)
