{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes #-}
module Algebra.Graph.Util (
    transpose, vertexSet, toList, gmap, mergeVertices, box, induce,
    removeVertex, splitVertex
    ) where

import Data.List.Extra (nubOrd)
import qualified Data.Set as Set
import Data.Set (Set)
import Test.QuickCheck

import Algebra.Graph hiding (box)

-- Note: Transpose can only transpose polymorphic graphs.
newtype Transpose g = T { transpose :: g } deriving (Arbitrary, Eq, Show)

instance Graph g => Graph (Transpose g) where
    type Vertex (Transpose g) = Vertex g
    empty       = T empty
    vertex      = T . vertex
    overlay x y = T $ overlay (transpose x) (transpose y)
    connect x y = T $ connect (transpose y) (transpose x)

instance (Graph g, Num g) => Num (Transpose g) where
    fromInteger = T . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- Note: Derived Eq instance does not satisfy Graph laws
newtype ToList a = TL { toList :: [a] } deriving (Arbitrary, Show)

instance Graph (ToList a) where
     type Vertex (ToList a) = a
     empty       = TL $ []
     vertex  x   = TL $ [x]
     overlay x y = TL $ toList x ++ toList y
     connect x y = TL $ toList x ++ toList y

instance Num a => Num (ToList a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

newtype VertexSet a = VS { vertexSet :: Set a } deriving (Arbitrary, Eq, Show)

instance Ord a => Graph (VertexSet a) where
    type Vertex (VertexSet a) = a
    empty       = VS $ Set.empty
    vertex  x   = VS $ Set.singleton x
    overlay x y = VS $ vertexSet x `Set.union` vertexSet y
    connect x y = VS $ vertexSet x `Set.union` vertexSet y

instance (Num a, Ord a) => Num (VertexSet a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

newtype Simplify g = S { simplify :: g } deriving (Arbitrary, Eq, Show)

instance (Eq g, Graph g) => Graph (Simplify g) where
    type Vertex (Simplify g) = Vertex g
    empty       = S empty
    vertex      = S . vertex
    overlay x y = S $ simpleOverlay (simplify x) (simplify y)
    connect x y = S $ simpleConnect (simplify x) (simplify y)

simpleOverlay :: (Graph g, Eq g) => g -> g -> g
simpleOverlay x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = overlay x y

simpleConnect :: (Graph g, Eq g) => g -> g -> g
simpleConnect x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = connect x y

instance (Eq g, Graph g, Num g) => Num (Simplify g) where
    fromInteger = S . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

newtype GraphFunctor g a = GF { gfor :: (a -> Vertex g) -> g }

gmap :: (a -> Vertex g) -> GraphFunctor g a -> g
gmap = flip gfor

mergeVertices :: (Vertex g -> Bool) -> Vertex g -> GraphFunctor g (Vertex g) -> g
mergeVertices p v = gmap $ \u -> if p u then v else u

instance Graph g => Graph (GraphFunctor g a) where
    type Vertex (GraphFunctor g a) = a
    empty       = GF $ \_ -> empty
    vertex  x   = GF $ \f -> vertex (f x)
    overlay x y = GF $ \f -> gmap f x `overlay` gmap f y
    connect x y = GF $ \f -> gmap f x `connect` gmap f y

instance (Graph g, Num a) => Num (GraphFunctor g a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- TODO: Fix type inference at the use site
box :: (Ord u, Ord v, Graph c, Vertex c ~ (u, v))
    => (forall a. (Graph a, Vertex a ~ u) => a)
    -> (forall b. (Graph b, Vertex b ~ v) => b) -> c
box x y = overlays $ xs ++ ys
  where
    xs = map (\b -> gmap (,b) x) . nubOrd $ toList y
    ys = map (\a -> gmap (a,) y) . nubOrd $ toList x

newtype GraphMonad g a = GM { bind :: (a -> g) -> g }

induce :: Graph g => (Vertex g -> Bool) -> GraphMonad g (Vertex g) -> g
induce p g = bind g $ \v -> if p v then vertex v else empty

removeVertex :: (Eq (Vertex g), Graph g) => Vertex g -> GraphMonad g (Vertex g) -> g
removeVertex v = induce (/= v)

splitVertex :: (Eq (Vertex g), Graph g) => Vertex g -> [Vertex g] -> GraphMonad g (Vertex g) -> g
splitVertex v vs g = bind g $ \u -> if u == v then vertices vs else vertex u

instance Graph g => Graph (GraphMonad g a) where
    type Vertex (GraphMonad g a) = a
    empty       = GM $ \_ -> empty
    vertex  x   = GM $ \f -> f x
    overlay x y = GM $ \f -> bind x f `overlay` bind y f
    connect x y = GM $ \f -> bind x f `connect` bind y f

instance (Graph g, Num a) => Num (GraphMonad g a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id
