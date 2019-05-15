{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Algebra.Graph.Acyclic.AdjacencyIntMap where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Proxy (Proxy(..))
import GHC.TypeLits

data Edge (f :: Nat) (t :: Nat) =
  Edge

newtype AdjacencyIntMap (edgesFollowRule :: Bool) =
  AM (IntMap IntSet)
  deriving (Show)

empty :: AdjacencyIntMap 'True
empty = AM IntMap.empty

singleton :: Int -> AdjacencyIntMap 'True
singleton x = AM $ IntMap.singleton x IntSet.empty

addEdge ::
     forall f t. (KnownNat f, KnownNat t)
  => Edge f t
  -> AdjacencyIntMap 'True
  -> AdjacencyIntMap (f <=? t)
addEdge _ (AM m) =
  AM $ IntMap.insertWith IntSet.union fI (IntSet.singleton tI) m
  where
    fI = fromIntegral . natVal $ (Proxy :: Proxy f)
    tI = fromIntegral . natVal $ (Proxy :: Proxy t)

-- REPL testing
graph :: AdjacencyIntMap 'True
graph =
  addEdge (Edge :: Edge 5 7) .
  addEdge (Edge :: Edge 4 5) . addEdge (Edge :: Edge 3 4) $
  empty
