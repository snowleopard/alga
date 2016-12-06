{-# LANGUAGE TypeFamilies #-}
module Algebra.Graph.Relation (Relation (..)) where

import           Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Graph

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

instance Ord a => Graph (Relation a) where
    type Vertex (Relation a) = a
    empty       = Relation Set.empty Set.empty
    vertex  x   = Relation (Set.singleton x) Set.empty
    overlay x y = Relation (domain   x `Set.union` domain   y)
                           (relation x `Set.union` relation y)
    connect x y = Relation (domain   x `Set.union` domain   y)
                           (relation x `Set.union` relation y
                            `Set.union` Set.fromDistinctAscList
                            [ (a, b) | a <- Set.elems (domain x)
                                     , b <- Set.elems (domain y) ])

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id
