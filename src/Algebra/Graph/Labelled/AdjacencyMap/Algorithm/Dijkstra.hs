module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Data.Map.Strict (Map, (!))
import Algebra.Graph.Label (Semiring(..), (<+>), zero)
import qualified Data.Map.Strict as Map
import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))

-- | Psuedocode for Generic-Single-Source-Shortest-Distance (G,s)
-- for i <- 1 to |Q|
--   do d[i] <- r[i] <- 0 
-- d[s] ← r[s] ← 1 
-- S ← {s} 
-- while S != []
--   do q <- head(S)
--      Dequeue(S)
--      r0 <- r[q]
--      r[q] <- 0
--      for each e in E[q]
--        do if d[n[e]] != d[n[e]] <+> (r0 <.> w[e])
--             then d[n[e]] <- d[n[e]] <+> (r0 <.> w[e])
--                  r[n[e]] <- r[n[e]] <+> (r0 <.> w[e])
--                  if n[e] not in S
--                    then Enqueue(S, n[e])
-- d[s] ← 1
--
-- Brief:
-- We use a queue S to maintain the set of vertices whose leaving edges are to be relaxed. S is initialized to [source].
-- We maintain 2 attributes, namely, `d` and `r`.
-- d[q] an estimate of the shortest distance from s to q
-- r[q] is the total weight added to d[q] since the last time q was extracted for S.
-- We update d and r till the queue is empty.

dijkstra ::
     (Ord a, Eq b, Semiring b) => a -> AdjacencyMap b a -> Map a b
dijkstra src adjMap = fst3 fDRQ
  where
    iDRQ = initialize src adjMap
    mDRQ = recursiveUpdateByQ adjMap iDRQ
    fDRQ = finalize src mDRQ
    fst3 (x, _, _) = x

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
