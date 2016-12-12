{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.Basic (Basic (..), Undirected (..), Transitive (..)) where

import Test.QuickCheck

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

instance Arbitrary a => Arbitrary (Basic a) where
    arbitrary = sized graph
      where
        graph 0 = return Empty
        graph 1 = Vertex <$> arbitrary
        graph n = do
            left <- choose (0, n)
            oneof [ Overlay <$> (graph left) <*> (graph $ n - left)
                  , Connect <$> (graph left) <*> (graph $ n - left) ]

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
    x == y = toRelation x == toRelation y

toRelation :: Ord a => Basic a -> Relation a
toRelation = foldBasic

foldBasic :: (Vertex g ~ a, Graph g) => Basic a -> g
foldBasic Empty         = empty
foldBasic (Vertex  x  ) = vertex x
foldBasic (Overlay x y) = overlay (foldBasic x) (foldBasic y)
foldBasic (Connect x y) = connect (foldBasic x) (foldBasic y)

newtype Undirected a = Undirected { fromUndirected :: Basic a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (Undirected a) where
    x == y = toSymmetricRelation x == toSymmetricRelation y

toSymmetricRelation :: Ord a => Undirected a -> Relation a
toSymmetricRelation = symmetricClosure . toRelation . fromUndirected

newtype Transitive a = Transitive { fromTransitive :: Basic a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (Transitive a) where
    x == y = toTransitiveRelation x == toTransitiveRelation y

toTransitiveRelation :: Ord a => Transitive a -> Relation a
toTransitiveRelation = transitiveClosure . toRelation . fromTransitive

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Undirected a) where
    type Vertex (Undirected a) = a
    empty       = Undirected empty
    vertex      = Undirected . vertex
    overlay x y = Undirected $ overlay (fromUndirected x) (fromUndirected y)
    connect x y = Undirected $ connect (fromUndirected x) (fromUndirected y)

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Transitive a) where
    type Vertex (Transitive a) = a
    empty       = Transitive empty
    vertex      = Transitive . vertex
    overlay x y = Transitive $ overlay (fromTransitive x) (fromTransitive y)
    connect x y = Transitive $ connect (fromTransitive x) (fromTransitive y)
