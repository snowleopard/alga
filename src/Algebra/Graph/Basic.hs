{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.Basic (
    Basic (..), fold, Reflexive, Undirected, PartialOrder
    ) where

import Control.Monad
import Test.QuickCheck

import Algebra.Graph
import Algebra.Graph.AdjacencyMap hiding (transpose)
import Algebra.Graph.Relation
import Algebra.Graph.Util

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

instance Arbitrary a => Arbitrary (Basic a) where
    arbitrary = arbitraryGraph

    shrink Empty         = []
    shrink (Vertex    _) = [Empty]
    shrink (Overlay x y) = [Empty, x, y]
                        ++ [Overlay x' y' | (x', y') <- shrink (x, y) ]
    shrink (Connect x y) = [Empty, x, y, Overlay x y]
                        ++ [Connect x' y' | (x', y') <- shrink (x, y) ]

instance Num a => Num (Basic a) where
    fromInteger = Vertex . fromInteger
    (+)         = Overlay
    (*)         = Connect
    signum      = const Empty
    abs         = id
    negate      = id

instance Ord a => Eq (Basic a) where
    x == y = toAdjacencyMap x == toAdjacencyMap y

toAdjacencyMap :: Ord a => Basic a -> AdjacencyMap a
toAdjacencyMap = fold

instance Applicative Basic where
    pure  = vertex
    (<*>) = ap

instance Monad Basic where
    return = vertex
    (>>=)  = flip foldMapBasic

fold :: (Vertex g ~ a, Graph g) => Basic a -> g
fold = foldMapBasic vertex

foldMapBasic :: (Vertex g ~ b, Graph g) => (a -> g) -> Basic a -> g
foldMapBasic _ Empty         = empty
foldMapBasic f (Vertex  x  ) = f x
foldMapBasic f (Overlay x y) = overlay (foldMapBasic f x) (foldMapBasic f y)
foldMapBasic f (Connect x y) = connect (foldMapBasic f x) (foldMapBasic f y)

newtype Reflexive a = R { fromReflexive :: Basic a }
    deriving (Arbitrary, Functor, Foldable, Num, Show)

instance Ord a => Eq (Reflexive a) where
    x == y = toReflexiveRelation x == toReflexiveRelation y

toReflexiveRelation :: Ord a => Reflexive a -> Relation a
toReflexiveRelation = reflexiveClosure . fold . fromReflexive

newtype Undirected a = U { fromUndirected :: Basic a }
    deriving (Arbitrary, Functor, Foldable, Num, Show)

instance Ord a => Eq (Undirected a) where
    x == y = bidirect x == bidirect y

bidirect :: Undirected a -> Basic a
bidirect (U g) = g `overlay` transpose (fold g)

newtype PartialOrder a = PO { fromPartialOrder :: Basic a }
    deriving (Arbitrary, Functor, Foldable, Num, Show)

instance Ord a => Eq (PartialOrder a) where
    x == y = toTransitiveRelation x == toTransitiveRelation y

toTransitiveRelation :: Ord a => PartialOrder a -> Relation a
toTransitiveRelation = transitiveClosure . fold . fromPartialOrder

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Reflexive a) where
    type Vertex (Reflexive a) = a
    empty       = R empty
    vertex      = R . vertex
    overlay x y = R $ fromReflexive x `overlay` fromReflexive y
    connect x y = R $ fromReflexive x `connect` fromReflexive y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Undirected a) where
    type Vertex (Undirected a) = a
    empty       = U empty
    vertex      = U . vertex
    overlay x y = U $ fromUndirected x `overlay` fromUndirected y
    connect x y = U $ fromUndirected x `connect` fromUndirected y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (PartialOrder a) where
    type Vertex (PartialOrder a) = a
    empty       = PO empty
    vertex      = PO . vertex
    overlay x y = PO $ fromPartialOrder x `overlay` fromPartialOrder y
    connect x y = PO $ fromPartialOrder x `connect` fromPartialOrder y
