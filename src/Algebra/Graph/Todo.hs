{-# LANGUAGE OverloadedStrings #-}
module Algebra.Graph.Todo (Todo, todo, low, high, (><)) where

import Data.String

import Algebra.Graph
import Algebra.Graph.Util

data Todo a = T [a] [a] (TopSort a) deriving Show

instance Ord a => Eq (Todo a) where
    x == y = todo x == todo y

instance (IsString a, Ord a) => IsString (Todo a) where
    fromString = vertex . fromString

low :: Ord a => a -> Todo a
low x = T [x] [] $ vertex x

high :: Ord a => a -> Todo a
high x = T [] [x] $ vertex x

(><) :: Ord a => a -> a -> Todo a
x >< y = low x `connect` high y

todo :: forall a. Ord a => Todo a -> Maybe [a]
todo (T l h g) = fmap (map snd) . topSort $ mapVertices prioritise g
  where
    prioritise :: a -> (Int, a)
    prioritise x
        | x `elem` h = (-1, x)
        | x `elem` l = ( 1, x)
        | otherwise  = ( 0, x)

instance (IsString a, Ord a) => Num (Todo a) where
    fromInteger i = fromString $ show (fromInteger i :: Integer)
    (+)           = overlay
    (*)           = connect
    signum        = const empty
    abs           = id
    negate        = id

instance Ord a => Graph (Todo a) where
    type Vertex (Todo a) = a
    empty  = T [] [] empty
    vertex = T [] [] . vertex
    overlay (T l1 h1 g1) (T l2 h2 g2) = T (l1 ++ l2) (h1 ++ h2) (overlay g1 g2)
    connect (T l1 h1 g1) (T l2 h2 g2) = T (l1 ++ l2) (h1 ++ h2) (connect g1 g2)
