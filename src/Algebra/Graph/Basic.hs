{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.Basic (
    Basic (..), Reflexive (..), Undirected (..), PartialOrder (..)
    ) where

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
    x == y = toRelation x == toRelation y

toRelation :: Ord a => Basic a -> Relation a
toRelation = foldBasic

foldBasic :: (Vertex g ~ a, Graph g) => Basic a -> g
foldBasic Empty         = empty
foldBasic (Vertex  x  ) = vertex x
foldBasic (Overlay x y) = overlay (foldBasic x) (foldBasic y)
foldBasic (Connect x y) = connect (foldBasic x) (foldBasic y)

newtype Reflexive a = Reflexive { fromReflexive :: Basic a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (Reflexive a) where
    x == y = toReflexiveRelation x == toReflexiveRelation y

toReflexiveRelation :: Ord a => Reflexive a -> Relation a
toReflexiveRelation = reflexiveClosure . toRelation . fromReflexive

newtype Undirected a = Undirected { fromUndirected :: Basic a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (Undirected a) where
    x == y = toSymmetricRelation x == toSymmetricRelation y

toSymmetricRelation :: Ord a => Undirected a -> Relation a
toSymmetricRelation = symmetricClosure . toRelation . fromUndirected

newtype PartialOrder a = PartialOrder { fromPartialOrder :: Basic a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (PartialOrder a) where
    x == y = toTransitiveRelation x == toTransitiveRelation y

toTransitiveRelation :: Ord a => PartialOrder a -> Relation a
toTransitiveRelation = transitiveClosure . toRelation . fromPartialOrder

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Reflexive a) where
    type Vertex (Reflexive a) = a
    empty       = Reflexive empty
    vertex      = Reflexive . vertex
    overlay x y = Reflexive $ fromReflexive x `overlay` fromReflexive y
    connect x y = Reflexive $ fromReflexive x `connect` fromReflexive y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (Undirected a) where
    type Vertex (Undirected a) = a
    empty       = Undirected empty
    vertex      = Undirected . vertex
    overlay x y = Undirected $ fromUndirected x `overlay` fromUndirected y
    connect x y = Undirected $ fromUndirected x `connect` fromUndirected y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Graph (PartialOrder a) where
    type Vertex (PartialOrder a) = a
    empty       = PartialOrder empty
    vertex      = PartialOrder . vertex
    overlay x y = PartialOrder $ fromPartialOrder x `overlay` fromPartialOrder y
    connect x y = PartialOrder $ fromPartialOrder x `connect` fromPartialOrder y
