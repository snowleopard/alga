{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Algebra.Graph.Acyclic.AdjacencyIntMap where

import qualified Algebra.Graph.AdjacencyIntMap as AIM
import Data.Proxy (Proxy(..))
import GHC.TypeLits

data Edge (f :: Nat) (t :: Nat) =
  Edge

newtype AdjacencyIntMap =
  AM AIM.AdjacencyIntMap
  deriving (Show)

empty :: AdjacencyIntMap
empty = AM AIM.empty

singleton :: Int -> AdjacencyIntMap
singleton x = AM $ AIM.vertex x

edge ::
     forall f t. (KnownNat f, KnownNat t, (f <=? t) ~ 'True)
  => Edge f t
  -> AdjacencyIntMap
edge _ = AM $ AIM.edge fI tI
  where
    fI = fromIntegral . natVal $ (Proxy :: Proxy f)
    tI = fromIntegral . natVal $ (Proxy :: Proxy t)

overlay :: AdjacencyIntMap -> AdjacencyIntMap -> AdjacencyIntMap
overlay (AM m1) (AM m2) = AM $ AIM.overlay m1 m2

edges ::
     forall f t. (KnownNat f, KnownNat t, (f <=? t) ~ 'True)
  => [Edge f t]
  -> AdjacencyIntMap
edges es = AM . AIM.edges $ map edgeTuple es

edgeTuple ::
     forall f t. (KnownNat f, KnownNat t, (f <=? t) ~ 'True)
  => Edge f t
  -> (Int, Int)
edgeTuple _ = (fI, tI)
  where
    fI = fromIntegral . natVal $ (Proxy :: Proxy f)
    tI = fromIntegral . natVal $ (Proxy :: Proxy t)

-- REPL testing
graph :: AdjacencyIntMap
graph = overlay (edge (Edge :: Edge 5 7)) (edge (Edge :: Edge 4 5))
