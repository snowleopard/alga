{-# LANGUAGE RankNTypes #-}
module Algebra.Graph.Util (
    transpose, VertexSet, vertexSet, ToList, toList, simplify, gmap,
    mergeVertices, box, bind, induce, removeVertex, splitVertex, removeEdge,
    deBruijn
    ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Algebra.Graph

newtype Fold a = F { fold :: forall g. Graph g => g -> (a -> g) -> (g -> g -> g) -> (g -> g -> g) -> g}

foldg :: Graph g => g -> (a -> g) -> (g -> g -> g) -> (g -> g -> g) -> Fold a -> g
foldg e v o c g = fold g e v o c

instance Graph (Fold a) where
    type Vertex (Fold a) = a
    empty       = F $ \e _ _ _ -> e
    vertex  x   = F $ \_ v _ _ -> v x
    overlay x y = F $ \e v o c -> fold x e v o c `o` fold y e v o c
    connect x y = F $ \e v o c -> fold x e v o c `c` fold y e v o c

instance Num a => Num (Fold a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

transpose :: Graph g => Fold (Vertex g) -> g
transpose = foldg empty vertex overlay (flip connect)

-- Note: Derived Eq instance does not satisfy Graph laws
newtype ToList a = ToList { toList :: [a] } deriving Show

instance Graph (ToList a) where
     type Vertex (ToList a) = a
     empty       = ToList $ []
     vertex  x   = ToList $ [x]
     overlay x y = ToList $ toList x ++ toList y
     connect x y = ToList $ toList x ++ toList y

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

simplify :: (Eq g, Graph g) => Fold (Vertex g) -> g
simplify = foldg empty vertex simpleOverlay simpleConnect

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

gmap :: Graph g => (a -> Vertex g) -> Fold a -> g
gmap f = foldg empty (vertex . f) overlay connect

mergeVertices :: Graph g => (Vertex g -> Bool) -> Vertex g -> Fold (Vertex g) -> g
mergeVertices p v = gmap $ \u -> if p u then v else u

-- Note: `gmap id` is needed
box :: (Graph g, Vertex g ~ (u, v)) => Fold u -> Fold v -> g
box x y = overlays $ xs ++ ys
  where
    xs = map (\b -> gmap (,b) x) . toList $ gmap id y
    ys = map (\a -> gmap (a,) y) . toList $ gmap id x

bind :: Graph g => Fold a -> (a -> g) -> g
bind g f = foldg empty f overlay connect g

induce :: Graph g => (Vertex g -> Bool) -> Fold (Vertex g) -> g
induce p g = bind g $ \v -> if p v then vertex v else empty

removeVertex :: (Eq (Vertex g), Graph g) => Vertex g -> Fold (Vertex g) -> g
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

splitVertex :: (Eq (Vertex g), Graph g) => Vertex g -> [Vertex g] -> Fold (Vertex g) -> g
splitVertex v vs g = bind g $ \u -> if u == v then vertices vs else vertex u

deBruijn :: (Graph g, Vertex g ~ [a]) => Int -> [a] -> g
deBruijn len alphabet = bind skeleton expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = edges    [        (Left s, Right s)   | s <- overlaps ]
    expand v = vertices [ either ([a] ++) (++ [a]) v | a <- alphabet ]
