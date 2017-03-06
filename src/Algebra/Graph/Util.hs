{-# LANGUAGE RankNTypes #-}
module Algebra.Graph.Util (
    transpose, VertexSet, vertexSet, ToList, toList, simplify, gmap,
    mergeVertices, box, bind, induce, removeVertex, splitVertex, removeEdge,
    deBruijn
    ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Algebra.Graph

-- Note: Transpose can only transpose polymorphic graphs.
newtype Transpose g = T { transpose :: g } deriving (Eq, Show)

instance Graph g => Graph (Transpose g) where
    type Vertex (Transpose g) = Vertex g
    empty       = T empty
    vertex      = T . vertex
    overlay x y = T $ transpose x `overlay` transpose y
    connect x y = T $ transpose y `connect` transpose x

instance (Graph g, Num g) => Num (Transpose g) where
    fromInteger = T . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- Note: Derived Eq instance does not satisfy Graph laws
newtype ToList a = TL { toList :: [a] } deriving Show

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

newtype VertexSet a = VS { vertexSet :: Set a } deriving (Eq, Show)

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

newtype Simplify g = S { simplify :: g } deriving (Eq, Show)

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

newtype GraphFunctor a = GF { gfor :: forall g. Graph g => (a -> Vertex g) -> g }

instance Graph (GraphFunctor a) where
    type Vertex (GraphFunctor a) = a
    empty       = GF $ \_ -> empty
    vertex  x   = GF $ \f -> vertex (f x)
    overlay x y = GF $ \f -> gmap f x `overlay` gmap f y
    connect x y = GF $ \f -> gmap f x `connect` gmap f y

instance Num a => Num (GraphFunctor a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

gmap :: Graph g => (a -> Vertex g) -> GraphFunctor a -> g
gmap = flip gfor

mergeVertices :: Graph g => (Vertex g -> Bool) -> Vertex g -> GraphFunctor (Vertex g) -> g
mergeVertices p v = gmap $ \u -> if p u then v else u

-- Note: `gmap id` is needed
box :: (Graph g, Vertex g ~ (u, v)) => GraphFunctor u -> GraphFunctor v -> g
box x y = overlays $ xs ++ ys
  where
    xs = map (\b -> gmap (,b) x) . toList $ gmap id y
    ys = map (\a -> gmap (a,) y) . toList $ gmap id x

newtype GraphMonad a = GM { bind :: forall g. Graph g => (a -> g) -> g }

instance Graph (GraphMonad a) where
    type Vertex (GraphMonad a) = a
    empty       = GM $ \_ -> empty
    vertex  x   = GM $ \f -> f x
    overlay x y = GM $ \f -> bind x f `overlay` bind y f
    connect x y = GM $ \f -> bind x f `connect` bind y f

instance Num a => Num (GraphMonad a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

induce :: Graph g => (Vertex g -> Bool) -> GraphMonad (Vertex g) -> g
induce p g = bind g $ \v -> if p v then vertex v else empty

removeVertex :: (Eq (Vertex g), Graph g) => Vertex g -> GraphMonad (Vertex g) -> g
removeVertex v = induce (/= v)

data Pieces g = Pieces { removeS :: g, removeT :: g, removeST :: g }

newtype Smash a = SM { smash :: forall g. (Vertex g ~ a, Graph g) => a -> a -> Pieces g }

instance Eq a => Graph (Smash a) where
    type Vertex (Smash a) = a
    empty       = SM $ \_ _ -> Pieces empty empty empty
    vertex x    = SM $ \s t -> Pieces (if x == s then empty else vertex x)
                                      (if x == t then empty else vertex x)
                                      (vertex x)
    overlay x y = SM $ \s t -> let px = smash x s t
                                   py = smash y s t
                               in Pieces (removeS  px `overlay` removeS  py)
                                         (removeT  px `overlay` removeT  py)
                                         (removeST px `overlay` removeST py)
    connect x y = SM $ \s t -> let px = smash x s t
                                   py = smash y s t
                               in Pieces (removeS  px `connect` removeS  py)
                                         (removeT  px `connect` removeT  py)
                                         ((removeS px `connect` removeST py) `overlay`
                                         (removeST px `connect` removeT  py))

instance (Eq a, Num a) => Num (Smash a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

removeEdge :: (Eq (Vertex g), Graph g) => Vertex g -> Vertex g -> Smash (Vertex g) -> g
removeEdge s t g = removeST $ smash g s t

splitVertex :: (Eq (Vertex g), Graph g) => Vertex g -> [Vertex g] -> GraphMonad (Vertex g) -> g
splitVertex v vs g = bind g $ \u -> if u == v then vertices vs else vertex u

deBruijn :: (Graph g, Vertex g ~ [a]) => Int -> [a] -> g
deBruijn len alphabet = bind skeleton expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = edges    [        (Left s, Right s)   | s <- overlaps ]
    expand v = vertices [ either ([a] ++) (++ [a]) v | a <- alphabet ]
