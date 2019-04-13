module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Data.Map.Strict (Map, (!))
import Algebra.Graph.Label (Semiring(..), (<+>), zero)
import qualified Data.Map.Strict as Map
import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))

dijkstra ::
     (Ord a, Eq b, Semiring b) => a -> AdjacencyMap b a -> (Map a b, Map a b, [a])
dijkstra src adjMap = fDRQ
  where
    iDRQ = initialize src adjMap
    mDRQ = recursiveUpdateByQ adjMap iDRQ
    fDRQ = finalize src mDRQ

initialize ::
     (Ord a, Semiring b) => a -> AdjacencyMap b a -> (Map a b, Map a b, [a])
initialize src adjMap = (srcOne, srcOne, [src])
  where
    allZero = Map.map (const zero) $ adjacencyMap adjMap
    srcOne = Map.insert src one allZero

finalize ::
     (Ord a, Semiring b)
  => a
  -> (Map a b, Map a b, [a])
  -> (Map a b, Map a b, [a])
finalize s (d, r, queue) = (Map.insert s one d, r, queue)

recursiveUpdateByQ ::
     (Ord a, Eq b, Semiring b)
  => AdjacencyMap b a
  -> (Map a b, Map a b, [a])
  -> (Map a b, Map a b, [a])
recursiveUpdateByQ adjMap dRQ@(d, r, queue) =
  case queue of
    [] -> dRQ
    (k:rest) -> recursiveUpdateByQ adjMap $ updateDRQByEdgeMap r' edgeMap cDRQ
      where r' = r ! k
            newR = Map.insert k zero r
            edgeMap = adjacencyMap adjMap ! k
            cDRQ = (d, newR, rest)

updateDRQByEdgeMap ::
     (Ord a, Eq b, Semiring b)
  => b
  -> Map a b
  -> (Map a b, Map a b, [a])
  -> (Map a b, Map a b, [a])
updateDRQByEdgeMap r' edgeMap pDRQ =
  foldr (\k dRQ -> updateDRQByEdge r' (k, edgeMap ! k) dRQ) pDRQ $
  Map.keys edgeMap

updateDRQByEdge ::
     (Ord a, Eq b, Semiring b)
  => b
  -> (a, b)
  -> (Map a b, Map a b, [a])
  -> (Map a b, Map a b, [a])
updateDRQByEdge r' (k, w) dRQ@(d, r, queue) =
  if pDK /= nDK
    then (Map.insert k nDK d, Map.insert k nRK r, nQ)
    else dRQ
  where
    pDK = d ! k
    nDK = pDK <+> (r' <.> w)
    nRK = (r ! k) <+> (r' <.> w)
    nQ =
      if k `elem` queue
        then queue
        else queue ++ [k]
