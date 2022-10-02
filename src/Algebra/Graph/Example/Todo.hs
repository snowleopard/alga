{-# LANGUAGE OverloadedStrings #-}
module Algebra.Graph.Example.Todo (
    -- * Creating and manipulating to-do lists
    Todo, todo, low, high, (~*~), (>*<), priority,

    -- * Examples
    shopping, holiday
    ) where

-- Based on https://blogs.ncl.ac.uk/andreymokhov/graphs-in-disguise/

import Data.Map (Map)
import Data.String

import Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.AdjacencyMap.Algorithm as AM
import Algebra.Graph.Class as C

import qualified Data.Map as Map

data Todo a = T (Map a Int) (AdjacencyMap a) deriving Show

instance Ord a => Eq (Todo a) where
    x == y = todo x == todo y

instance (IsString a, Ord a) => IsString (Todo a) where
    fromString = C.vertex . fromString

-- Lower the priority of items in a given todo list
low :: Todo a -> Todo a
low (T p g) = T (Map.map (subtract 1) p) g

-- Raise the priority of items in a given todo list
high :: Todo a -> Todo a
high (T p g) = T (Map.map (+1) p) g

-- Specify exact priority of items in a given todo list (default 0)
priority :: Int -> Todo a -> Todo a
priority x (T p g) = T (Map.map (const x) p) g

-- Pull the arguments together as close as possible
(~*~) :: Ord a => Todo a -> Todo a -> Todo a
x ~*~ y = low x `C.connect` high y

-- Repel the arguments as far as possible
(>*<) :: Ord a => Todo a -> Todo a -> Todo a
x >*< y = high x `C.connect` low y

todo :: forall a. Ord a => Todo a -> Maybe [a]
todo (T p g) = case AM.topSort $ gmap prioritise g of
    Left _ -> Nothing
    Right xs -> Just $ map snd xs
  where
    prioritise :: a -> (Int, a)
    prioritise x = (negate $ Map.findWithDefault 0 x p, x)

instance (IsString a, Ord a) => Num (Todo a) where
    fromInteger i = fromString $ show (fromInteger i :: Integer)
    (+)           = C.overlay
    (*)           = C.connect
    signum        = const C.empty
    abs           = id
    negate        = id

instance Ord a => Graph (Todo a) where
    type Vertex (Todo a) = a
    empty    = T Map.empty AM.empty
    vertex x = T (Map.singleton x 0) (C.vertex x)
    overlay (T p1 g1) (T p2 g2) = T (Map.unionWith (+) p1 p2) (C.overlay g1 g2)
    connect (T p1 g1) (T p2 g2) = T (Map.unionWith (+) p1 p2) (C.connect g1 g2)

-- λ> todo shopping
-- Just ["coat","presents","phone wife","scarf"]
shopping :: Todo String
shopping = "presents" + "coat" + "phone wife" ~*~ "scarf"

-- λ> todo holiday
-- Just ["coat","presents","phone wife","scarf","pack","travel"]
holiday :: Todo String
holiday = shopping * "pack" * "travel"
