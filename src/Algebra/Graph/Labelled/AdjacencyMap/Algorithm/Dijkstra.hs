module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Algebra.Graph.Label (Capacity, Distance, Semiring(..), (<+>), zero)
import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))
import Control.Monad (sequence_, when)
import Control.Monad.Trans.State.Strict (State, execState, get, gets, put)
import Data.Heap (FstMaxPolicy, FstMinPolicy, Heap, HeapItem(..))
import qualified Data.Heap as Heap
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

type DijkstraForDistance a b = DijkstraState FstMinPolicy a (Distance b)

type DijkstraForCapacity a b = DijkstraState FstMaxPolicy a (Capacity b)

dijkstra ::
     (Eq e, Ord a, HeapItem p (e, a), Semiring e)
  => a
  -> AdjacencyMap e a
  -> DijkstraState p a e
dijkstra a am =
  flip execState (DijkstraState Heap.empty Map.empty Map.empty) $ do
    initialize a am
    exploreVertices am

data DijkstraState p a e = DijkstraState
  { heap :: Heap p (e, a)
  , distance :: Map a e
  , path :: Map a (Maybe a)
  }

instance (Show e, Show a) => Show (DijkstraState p a e) where
  show (DijkstraState heap' distance' path') =
    "distance: " ++ show distance' ++ "\n" ++ "path: " ++ show path'

putHeap :: Heap p (e, a) -> State (DijkstraState p a e) ()
putHeap h = do
  (DijkstraState _ distance' path') <- get
  put $ DijkstraState h distance' path'

initialize ::
     (Ord a, HeapItem p (e, a), Semiring e)
  => a
  -> AdjacencyMap e a
  -> State (DijkstraState p a e) ()
initialize a am =
  put $
  DijkstraState
    (Heap.singleton (one, a))
    (Map.insert a one . Map.map (const zero) $ adjacencyMap am)
    (Map.map (const Nothing) $ adjacencyMap am)

exploreVertices ::
     (Ord a, Eq e, HeapItem p (e, a), Semiring e)
  => AdjacencyMap e a
  -> State (DijkstraState p a e) ()
exploreVertices am = do
  curHeap <- gets heap
  case Heap.view curHeap of
    Nothing -> return ()
    (Just ((_, v), t)) -> do
      putHeap t
      exploreVertex v am
      exploreVertices am

exploreVertex ::
     (Ord a, Semiring e, Eq e, HeapItem p (e, a))
  => a
  -> AdjacencyMap e a
  -> State (DijkstraState p a e) ()
exploreVertex v1 am =
  sequence_ $
  Map.mapWithKey (\v2 e -> exploreEdge e v1 v2) (adjacencyMap am ! v1)

modifyHeapItemPrio ::
     (HeapItem p (e, a), Eq a) => a -> e -> Heap p (e, a) -> Heap p (e, a)
modifyHeapItemPrio vertex prio curHeap =
  Heap.insert (prio, vertex) $ Heap.filter (isNot vertex) curHeap
  where
    isNot v (_, i) = v /= i

exploreEdge ::
     (HeapItem p (e, a), Ord a, Semiring e, Eq e)
  => e
  -> a
  -> a
  -> State (DijkstraState p a e) ()
exploreEdge e v1 v2 = do
  curHeap <- gets heap
  curDistance <- gets distance
  curPath <- gets path
  let curDV1 = curDistance ! v1
  let curDV2 = curDistance ! v2
  let newDV2 = curDV2 <+> (curDV1 <.> e)
  when (newDV2 /= curDV2) $
    put $
    DijkstraState
      (modifyHeapItemPrio v2 newDV2 curHeap)
      (Map.insert v2 newDV2 curDistance)
      (Map.insert v2 (Just v1) curPath)
