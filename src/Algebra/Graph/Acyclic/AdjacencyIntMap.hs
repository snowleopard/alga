{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Algebra.Graph.Acyclic.AdjacencyIntMap where

import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NAM
import Data.Proxy (Proxy(..))
import GHC.TypeLits

data Edge (f :: Nat) (t :: Nat) =
  Edge

newtype AdjacencyIntMap =
  AM (NAM.AdjacencyMap Int)
  deriving (Show)

singleton :: Int -> AdjacencyIntMap
singleton x = AM $ NAM.vertex x

edge ::
     forall f t. (KnownNat f, KnownNat t, (f <=? t) ~ 'True)
  => Edge f t
  -> AdjacencyIntMap
edge _ = AM $ NAM.edge fI tI
  where
    fI = fromIntegral . natVal $ (Proxy :: Proxy f)
    tI = fromIntegral . natVal $ (Proxy :: Proxy t)

overlay :: AdjacencyIntMap -> AdjacencyIntMap -> AdjacencyIntMap
overlay (AM m1) (AM m2) = AM $ NAM.overlay m1 m2

-- REPL testing
graph :: AdjacencyIntMap
graph = overlay (edge (Edge :: Edge 5 7)) (edge (Edge :: Edge 4 5))
